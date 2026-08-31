#!/usr/bin/env Rscript

# Robustez das especificações de matching da amostra principal S4.
# Compara matching com reposição, entropy matching, painel balanceado e
# propensity score. A base oficial é usada na estimação, mas não nas figuras.

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(did)
  library(ggplot2)
})

set.seed(3854035)
args <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

matching_covars <- c(
  "idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores",
  "idade_media_trabalhadores", "prop_homens", "prop_pretos"
)

specs <- copy(outcomes_produto3)

spec_levels <- c(
  "CS-Base", "Com reposição", "Entropy matching",
  "Painel balanceado", "Propensity score 1:1"
)

covars <- fread(
  file.path(data_dir, "produto3_covariaveis_matching.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8"
)
outcomes <- fread(
  file.path(data_dir, "produto3_outcomes_rais.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8"
)
official <- fread(
  file.path(data_dir, "produto3_unidades_matching.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8"
)
covars[, cnpj_raiz := clean_root(cnpj_raiz)]
outcomes[, cnpj_raiz := clean_root(cnpj_raiz)]
official[, cnpj_raiz := clean_root(cnpj_raiz)]
covars[, division := substr(as.character(as.integer(cnae_classe_principal)), 1, 2)]
outcomes <- unique(outcomes[ano >= 2020L & ano <= 2025L], by = c("cnpj_raiz", "ano"))

# Mantém as mesmas restrições de presença da S4 em cada coorte.
has_window <- function(ids, g0) {
  yrs <- if (g0 == 2024L) 2020:2025 else 2021:2025
  outcomes[cnpj_raiz %chin% ids & ano %in% yrs,
           .(n_years = uniqueN(ano)), by = cnpj_raiz][n_years == length(yrs), cnpj_raiz]
}

matching_pool <- function(g0, treated_roots, excluded_controls = character()) {
  available_roots <- unique(outcomes$cnpj_raiz)
  d <- unique(copy(covars[g == g0 & cnpj_raiz %chin% available_roots]), by = "cnpj_raiz")
  d <- d[complete.cases(d[, c(matching_covars, "division", "tamanho_estab_principal"), with = FALSE])]
  d <- d[(treat == 0L & !cnpj_raiz %chin% excluded_controls) | cnpj_raiz %chin% treated_roots]
  d <- d[cnpj_raiz %chin% has_window(d$cnpj_raiz, g0)]
  d[, treat := as.integer(cnpj_raiz %chin% treated_roots)]
  missing_treated <- setdiff(treated_roots, d$cnpj_raiz)
  if (length(missing_treated)) {
    stop(sprintf("Coorte %d perdeu %d tratadas ao reconstruir a S4", g0, length(missing_treated)))
  }
  d
}

selected_rows <- function(d, treated_roots, chosen, control_weights, g0) {
  ans <- rbindlist(list(
    data.table(cnpj_raiz = treated_roots, treat = 1L, weights = 1),
    data.table(cnpj_raiz = chosen, treat = 0L, weights = as.numeric(control_weights))
  ))
  ans[, coorte_match := g0]
  ans
}

entropy_weights <- function(X, target) {
  X <- as.matrix(X)
  sc <- apply(X, 2, sd)
  sc[!is.finite(sc) | sc < 1e-8] <- 1
  mu <- colMeans(X)
  Z <- sweep(sweep(X, 2, mu, "-"), 2, sc, "/")
  tt <- (target - mu) / sc
  objective <- function(lambda) {
    a <- drop(Z %*% lambda)
    m <- max(a)
    m + log(mean(exp(a - m))) - sum(lambda * tt)
  }
  fit <- optim(rep(0, ncol(Z)), objective, method = "BFGS",
               control = list(maxit = 1000, reltol = 1e-10))
  a <- drop(Z %*% fit$par)
  w <- exp(a - max(a))
  w / sum(w)
}

matching_cohorts <- sort(unique(official$coorte_match))
all_treated_roots <- unique(official[treat == 1L, cnpj_raiz])
replacement_parts <- list()
propensity_parts <- list()
entropy_parts <- list()
usage_parts <- list()
pool_parts <- list()
used_propensity <- character()
used_entropy <- character()

for (g0 in matching_cohorts) {
  treated_roots <- unique(official[coorte_match == g0 & treat == 1L, cnpj_raiz])

  # Mahalanobis 1:1 com reposição e setor exato.
  d_rep <- matching_pool(g0, treated_roots)
  d_rep_df <- as.data.frame(d_rep)
  rownames(d_rep_df) <- d_rep_df$cnpj_raiz
  fit_rep <- matchit(
    reformulate(matching_covars, "treat"), data = d_rep_df,
    method = "nearest", distance = "mahalanobis", exact = ~division,
    ratio = 1, replace = TRUE
  )
  chosen_rep <- as.character(fit_rep$match.matrix[, 1L])
  if (anyNA(chosen_rep)) stop(sprintf("Coorte %d contém tratada sem match com reposição", g0))
  use <- as.data.table(table(cnpj_raiz = chosen_rep))
  setnames(use, "N", "n_uses")
  use[, coorte_match := g0]
  usage_parts[[as.character(g0)]] <- use
  replacement_parts[[as.character(g0)]] <- selected_rows(
    d_rep, treated_roots, use$cnpj_raiz, use$n_uses, g0
  )

  # Propensity score 1:1 sem reposição, com setor exato. Controles já usados
  # em outra coorte e empresas tratadas em qualquer coorte ficam fora do pool.
  d_ps <- matching_pool(g0, treated_roots, c(used_propensity, all_treated_roots))
  d_ps_df <- as.data.frame(d_ps)
  rownames(d_ps_df) <- d_ps_df$cnpj_raiz
  fit_ps <- matchit(
    reformulate(matching_covars, "treat"), data = d_ps_df,
    method = "nearest", distance = "glm", link = "logit", exact = ~division,
    ratio = 1, replace = FALSE
  )
  chosen_ps <- as.character(fit_ps$match.matrix[, 1L])
  if (anyNA(chosen_ps)) stop(sprintf("Coorte %d contém tratada sem match por propensity score", g0))
  propensity_parts[[as.character(g0)]] <- selected_rows(
    d_ps, treated_roots, chosen_ps, rep(1, length(chosen_ps)), g0
  )
  used_propensity <- union(used_propensity, chosen_ps)

  # Entropy matching dentro de coorte e divisão. Seleciona N controles e
  # recalibra os pesos no suporte 1:1 final.
  d_ent <- matching_pool(g0, treated_roots, c(used_entropy, all_treated_roots))
  for (div0 in unique(d_ent[treat == 1L, division])) {
    tr <- d_ent[treat == 1L & division == div0]
    ct <- d_ent[treat == 0L & division == div0]
    n <- nrow(tr)
    if (n == 0L) next
    if (nrow(ct) < n) stop(sprintf("Entropia sem controles suficientes em %d/%s", g0, div0))
    w0 <- entropy_weights(ct[, ..matching_covars], colMeans(tr[, ..matching_covars]))
    sel <- ct[order(-w0)][seq_len(n)]
    w <- entropy_weights(sel[, ..matching_covars], colMeans(tr[, ..matching_covars])) * n
    key <- paste(g0, div0, sep = "|")
    entropy_parts[[key]] <- selected_rows(d_ent, tr$cnpj_raiz, sel$cnpj_raiz, w, g0)
    used_entropy <- union(used_entropy, sel$cnpj_raiz)
  }

  pool_parts[[as.character(g0)]] <- data.table(
    coorte_match = g0,
    tratadas = uniqueN(d_rep[treat == 1L, cnpj_raiz]),
    controles_candidatos = uniqueN(d_rep[treat == 0L, cnpj_raiz]),
    divisoes = uniqueN(d_rep$division)
  )
}

replacement <- rbindlist(replacement_parts, fill = TRUE)
propensity <- rbindlist(propensity_parts, fill = TRUE)
entropy <- rbindlist(entropy_parts, fill = TRUE)
usage <- rbindlist(usage_parts, fill = TRUE)
pools <- rbindlist(pool_parts)

make_units <- function(m) {
  m[, .(
    g = if (any(treat == 1L)) as.numeric(unique(coorte_match[treat == 1L])[1]) else 0,
    analysis_weight = if (any(treat == 1L)) 1 else sum(as.numeric(weights))
  ), by = cnpj_raiz]
}

official_units <- make_units(copy(official))
replacement_units <- make_units(copy(replacement))
entropy_units <- make_units(copy(entropy))
propensity_units <- make_units(copy(propensity))

# Painel balanceado preserva pares oficiais completos nos seis anos e é
# reconstruído separadamente para cada outcome.
balanced_rows <- function(outcome) {
  complete <- outcomes[, .(
    ok = uniqueN(ano) == 6L && all(2020:2025 %in% ano) && all(is.finite(get(outcome)))
  ), by = cnpj_raiz][ok == TRUE, cnpj_raiz]
  z <- copy(official)
  z[, pair_id := paste(coorte_match, subclass, sep = "|")]
  good_pairs <- z[, .(
    ok = .N == 2L && sum(treat == 1L) == 1L && sum(treat == 0L) == 1L &&
      all(cnpj_raiz %chin% complete)
  ), by = pair_id][ok == TRUE, pair_id]
  z[pair_id %chin% good_pairs][, pair_id := NULL]
}

estimate <- function(units, outcome, specification) {
  d <- merge(
    outcomes[, c("cnpj_raiz", "ano", outcome), with = FALSE],
    units, by = "cnpj_raiz"
  )
  setnames(d, outcome, "y")
  d <- d[is.finite(y)]
  d[, id_num := as.integer(factor(cnpj_raiz))]
  fit <- att_gt(
    yname = "y", tname = "ano", idname = "id_num", gname = "g",
    data = as.data.frame(d), weightsname = "analysis_weight", xformla = ~1,
    control_group = "nevertreated", base_period = "universal",
    panel = TRUE, allow_unbalanced_panel = TRUE, est_method = "dr",
    bstrap = FALSE, cband = FALSE, alp = .05, print_details = FALSE
  )
  es <- aggte(
    fit, type = "dynamic", min_e = -4, max_e = 1, na.rm = TRUE,
    bstrap = FALSE, cband = FALSE, alp = .05
  )
  dynamic <- data.table(
    specification, outcome, event_time = as.integer(es$egt),
    att = as.numeric(es$att.egt), se = as.numeric(es$se.egt)
  )
  dynamic[event_time == -1L, `:=`(att = 0, se = 0)]
  dynamic[, `:=`(low = att - qnorm(.975) * se, high = att + qnorm(.975) * se)]
  summary <- data.table(
    specification, outcome,
    att = as.numeric(es$overall.att), se = as.numeric(es$overall.se),
    p_value = 2 * pnorm(-abs(as.numeric(es$overall.att / es$overall.se))),
    pretest_p = as.numeric(fit$Wpval),
    n_treated = uniqueN(units[g > 0, cnpj_raiz]),
    n_controls = uniqueN(units[g == 0, cnpj_raiz]),
    control_weight = sum(units[g == 0, analysis_weight]),
    control_ess = units[g == 0, sum(analysis_weight)^2 / sum(analysis_weight^2)]
  )
  list(summary = summary, dynamic = dynamic)
}

estimates <- list()
balanced_samples <- list()
for (v in specs$outcome) {
  bal <- balanced_rows(v)
  balanced_samples[[v]] <- bal
  sample_units <- list(
    "CS-Base" = official_units,
    "Com reposição" = replacement_units,
    "Entropy matching" = entropy_units,
    "Painel balanceado" = make_units(copy(bal)),
    "Propensity score 1:1" = propensity_units
  )
  for (nm in names(sample_units)) {
    estimates[[paste(nm, v, sep = "|")]] <- estimate(sample_units[[nm]], v, nm)
  }
  message("Estimado: ", v)
}

att_summary <- rbindlist(lapply(estimates, `[[`, "summary"))
dynamic <- rbindlist(lapply(estimates, `[[`, "dynamic"))
att_summary <- merge(att_summary, specs, by = "outcome", all.x = TRUE)
dynamic <- merge(dynamic, specs, by = "outcome", all.x = TRUE)
att_summary[, specification := factor(specification, levels = spec_levels)]
dynamic[, specification := factor(specification, levels = spec_levels)]
setorder(att_summary, outcome, specification)

# Um event study por outcome, sem CS-Base: a comparação contém somente as
# quatro especificações alternativas de matching.
event_specs <- copy(outcomes_produto3)
event_levels <- spec_levels[spec_levels != "CS-Base"]
pd <- position_dodge(width = .20)
for (k in seq_len(nrow(event_specs))) {
  event_plot_data <- copy(dynamic[
    outcome == event_specs$outcome[k] & as.character(specification) != "CS-Base"
  ])
  event_plot_data[, specification := factor(as.character(specification), levels = event_levels)]
  p_event <- ggplot(
    event_plot_data,
    aes(event_time, att, color = specification, group = specification)
  ) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = .35) +
    geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
    geom_errorbar(aes(ymin = low, ymax = high), width = .07, position = pd, linewidth = .42) +
    geom_point(position = pd, size = 1.7) +
    scale_x_continuous(breaks = -4:1) +
    scale_color_manual(values = paleta_bndes[1:4], name = NULL) +
    labs(x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
    tema_bndes(legend_position = "bottom", base_size = 11)
  ggsave(
    file.path(output_dir, paste0("Robustez - Especificacoes de matching - Event study - ",
                                event_specs$safe[k], ".png")),
    p_event, width = 8.5, height = 5.2, dpi = 300, bg = cor_fundo
  )
}
print(att_summary[, .(label, specification, att, se, p_value, pretest_p,
                      n_treated, n_controls, control_ess)])
