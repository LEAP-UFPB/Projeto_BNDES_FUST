#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(MatchIt)
  library(did)
  library(ggplot2)
})

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- args_all[grepl("^--file=", args_all)]
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/")
produto_dir <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/")
repo_root <- normalizePath(file.path(produto_dir, ".."), winslash = "/")
out_dir <- file.path(produto_dir, "analysis", "psm_did")

covars <- fread(
  file.path(out_dir, "covariaveis_matching_firma_ano_v2_sem_outcomes.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character", municipio_principal = "character",
    subsetor_ibge_principal_codigo = "character", tamanho_estab_principal = "character")
)
panel_cache <- fread(
  file.path(out_dir, "painel_did_psm_rais_2021_2025_v2.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
outcomes <- unique(
  panel_cache[, .(cnpj_raiz, ano, log_emprego, log_massa_salarial)],
  by = c("cnpj_raiz", "ano")
)
available_roots <- unique(outcomes$cnpj_raiz)

fust <- as.data.table(read_excel(file.path(repo_root, "data", "Painel_CNPJ_FUST_RAIS_2018_2024.xlsx"), sheet = "painel_cnpj_ano"))
fust[, cnpj_raiz := sprintf("%08d", suppressWarnings(as.integer(gsub("\\D", "", as.character(cnpj_raiz)))))]
fust[, g_actual := as.integer(ano_primeira_operacao_fust)]
fust <- fust[!is.na(cnpj_raiz) & !is.na(g_actual), .(g_actual = min(g_actual)), by = cnpj_raiz]
fust_roots <- unique(fust$cnpj_raiz)

numeric_vars <- c("idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores", "idade_media_trabalhadores")
for (variable in numeric_vars) {
  covars[, (variable) := as.numeric(get(variable))]
  covars[, (variable) := fifelse(is.finite(get(variable)), get(variable), median(get(variable), na.rm = TRUE)), by = g]
}

specs <- CJ(
  distance = c("glm_linear", "glm_nonlinear", "mahalanobis"),
  ratio = c(1L, 3L, 5L),
  caliper = c(0.05, 0.10, 0.20),
  exact_level = c("municipio", "uf"),
  unique = TRUE
)
specs[distance == "mahalanobis", caliper := NA_real_]
specs <- unique(specs)
specs[, spec_id := .I]

weighted_mean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[is.finite(x)], na.rm = TRUE)

match_spec <- function(spec) {
  parts <- list()
  for (cohort in 2023:2025) {
    dt <- copy(covars[g == cohort & cnpj_raiz %in% available_roots])
    dt <- dt[(treat == 1) | (treat == 0 & !(cnpj_raiz %in% fust_roots))]
    dt <- dt[complete.cases(dt[, ..numeric_vars])]
    if (sum(dt$treat == 1) < 2 || sum(dt$treat == 0) < 2) next
    formula <- if (spec$distance == "glm_nonlinear") {
      treat ~ poly(idade_firma_pre, 2, raw = TRUE) + poly(prop_superior, 2, raw = TRUE) +
        poly(tempo_medio_trabalhadores, 2, raw = TRUE) + poly(idade_media_trabalhadores, 2, raw = TRUE) +
        idade_firma_pre:prop_superior + tempo_medio_trabalhadores:idade_media_trabalhadores
    } else {
      treat ~ idade_firma_pre + prop_superior + tempo_medio_trabalhadores + idade_media_trabalhadores
    }
    exact_formula <- if (spec$exact_level == "municipio") {
      ~ municipio_principal + broad_sector + tamanho_estab_principal
    } else {
      ~ uf_principal + subsetor_ibge_principal_codigo + tamanho_estab_principal
    }
    arguments <- list(
      formula = formula, data = dt, method = "nearest", replace = TRUE,
      ratio = spec$ratio, exact = exact_formula,
      distance = if (spec$distance == "mahalanobis") "mahalanobis" else "glm"
    )
    if (spec$distance != "mahalanobis") {
      arguments$caliper <- spec$caliper
      arguments$std.caliper <- TRUE
    }
    matched <- tryCatch(match.data(do.call(matchit, arguments), drop.unmatched = TRUE), error = function(e) NULL)
    if (is.null(matched)) next
    matched <- as.data.table(matched)[weights > 0]
    matched[, coorte_match := cohort]
    parts[[as.character(cohort)]] <- matched
  }
  rbindlist(parts, fill = TRUE)
}

evaluate_match <- function(matched) {
  if (!nrow(matched)) return(data.table())
  units <- matched[, .(treat = max(treat), weight = sum(weights)), by = .(coorte_match, cnpj_raiz)]
  leads <- merge(units, outcomes, by = "cnpj_raiz", allow.cartesian = TRUE)
  leads[, event_time := ano - coorte_match]
  leads <- leads[event_time <= -2]
  lead_stats <- rbindlist(lapply(c("log_emprego", "log_massa_salarial"), function(outcome) {
    baseline <- merge(
      leads[, .(cnpj_raiz, coorte_match, treat, weight, ano, event_time, value = get(outcome))],
      outcomes[, .(cnpj_raiz, ano_base = ano, baseline = get(outcome))], by = "cnpj_raiz", allow.cartesian = TRUE
    )[ano_base == coorte_match - 1]
    baseline[, change := value - baseline]
    baseline[, .(
      gap = weighted_mean(change[treat == 1], weight[treat == 1]) -
        weighted_mean(change[treat == 0], weight[treat == 0])
    ), by = .(coorte_match, event_time)]
  }), fill = TRUE)
  smds <- unlist(lapply(numeric_vars, function(variable) {
    treated <- matched[treat == 1]
    control <- matched[treat == 0]
    difference <- weighted_mean(treated[[variable]], treated$weights) - weighted_mean(control[[variable]], control$weights)
    pooled_sd <- sqrt((var(treated[[variable]]) + var(control[[variable]])) / 2)
    abs(difference / pooled_sd)
  }))
  data.table(
    pretrend_objective = mean(abs(lead_stats$gap), na.rm = TRUE),
    max_smd = max(smds, na.rm = TRUE),
    treated_matched = uniqueN(matched[treat == 1, cnpj_raiz]),
    controls_matched = uniqueN(matched[treat == 0, cnpj_raiz])
  )
}

audit <- list()
matches <- list()
for (index in seq_len(nrow(specs))) {
  spec <- specs[index]
  matched <- match_spec(spec)
  evaluation <- evaluate_match(matched)
  if (nrow(evaluation)) {
    audit[[length(audit) + 1L]] <- cbind(spec, evaluation)
    matches[[as.character(spec$spec_id)]] <- matched
  }
}
audit <- rbindlist(audit, fill = TRUE)
audit[, score := pretrend_objective + pmax(max_smd - 0.10, 0) + 10 / pmax(treated_matched, 1)]
setorder(audit, score, pretrend_objective, max_smd)
best <- audit[1]
matched <- matches[[as.character(best$spec_id)]]
fwrite(audit, file.path(out_dir, "auditoria_especificacoes_matching_pretrend.csv"), sep = ";", bom = TRUE)
fwrite(best, file.path(out_dir, "melhor_especificacao_matching_pretrend.csv"), sep = ";", bom = TRUE)

units <- matched[, .(treat = max(treat), match_weight = sum(weights)), by = .(coorte_match, cnpj_raiz)]
units[, g := fifelse(treat == 1, coorte_match, 0L)]
units[, unit_id := paste(coorte_match, cnpj_raiz, sep = "_")]
panel <- merge(outcomes, units, by = "cnpj_raiz", allow.cartesian = TRUE)
panel[, id_num := as.integer(factor(unit_id))]
cluster_bootstrap_reps <- 49L
cluster_bootstrap_seed <- 3854035L

run_outcome <- function(outcome, label, stub) {
  estimate_dynamic <- function(input) {
    cohort_effects <- rbindlist(lapply(sort(unique(input$coorte_match)), function(cohort) {
    dt <- copy(input[coorte_match == cohort])
    baseline <- dt[ano == cohort - 1, .(unit_id, baseline = get(outcome))]
    dt <- merge(dt, baseline, by = "unit_id")
    dt[, `:=`(change = get(outcome) - baseline, event_time = ano - cohort)]
    dt[, {
      treated_values <- change[treat == 1]
      treated_weights <- match_weight[treat == 1]
      control_values <- change[treat == 0]
      control_weights <- match_weight[treat == 0]
      treated_mean <- weighted.mean(treated_values, treated_weights)
      control_mean <- weighted.mean(control_values, control_weights)
      treated_var <- weighted.mean((treated_values - treated_mean)^2, treated_weights)
      control_var <- weighted.mean((control_values - control_mean)^2, control_weights)
      treated_neff <- sum(treated_weights)^2 / sum(treated_weights^2)
      control_neff <- sum(control_weights)^2 / sum(control_weights^2)
      .(
        att = treated_mean - control_mean,
        treated = uniqueN(unit_id[treat == 1])
      )
    }, by = event_time]
  }), fill = TRUE)
    dynamic <- cohort_effects[is.finite(att), {
    weights <- treated / sum(treated)
      .(att = sum(weights * att), cohorts = .N)
  }, by = event_time]
    dynamic[order(event_time)]
  }
  dynamic <- estimate_dynamic(panel)
  set.seed(cluster_bootstrap_seed)
  cluster_ids <- unique(panel$cnpj_raiz)
  bootstrap_draws <- matrix(NA_real_, nrow = cluster_bootstrap_reps, ncol = nrow(dynamic))
  colnames(bootstrap_draws) <- as.character(dynamic$event_time)
  for (rep in seq_len(cluster_bootstrap_reps)) {
    sampled <- sample(cluster_ids, length(cluster_ids), replace = TRUE)
    bootstrap_panel <- rbindlist(lapply(seq_along(sampled), function(index) {
      draw <- copy(panel[cnpj_raiz == sampled[index]])
      draw[, `:=`(
        cnpj_raiz = paste0(cnpj_raiz, "_boot_", index),
        unit_id = paste0(unit_id, "_boot_", index)
      )]
      draw
    }))
    bootstrap_estimate <- estimate_dynamic(bootstrap_panel)
    positions <- match(dynamic$event_time, bootstrap_estimate$event_time)
    bootstrap_draws[rep, !is.na(positions)] <- bootstrap_estimate$att[positions[!is.na(positions)]]
  }
  dynamic[, se := apply(bootstrap_draws, 2, sd, na.rm = TRUE)]
  dynamic[event_time == -1, se := 0]
  setorder(dynamic, event_time)
  dynamic[, `:=`(ci_low = att - 1.96 * se, ci_high = att + 1.96 * se, outcome = label)]
  plot <- ggplot(dynamic, aes(event_time, att)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -1, color = "grey60", linetype = "dashed", linewidth = 0.35) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8FB8CA", alpha = 0.25) +
    geom_line(color = "#1C5D73", linewidth = 0.8) + geom_point(color = "#1C5D73", size = 2.2) +
    scale_x_continuous(breaks = dynamic$event_time) +
    labs(title = paste0("Event study - ", label),
      x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
    theme_minimal(base_size = 12) + theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  ggsave(file.path(out_dir, paste0("event_study_", stub, ".png")), plot, width = 8, height = 5, dpi = 300, bg = "white")
  dynamic
}

results <- rbindlist(list(
  run_outcome("log_emprego", "Log emprego", "log_emprego"),
  run_outcome("log_massa_salarial", "Log massa salarial", "log_massa_salarial")
), fill = TRUE)
fwrite(results, file.path(out_dir, "att_event_study.csv"), sep = ";", bom = TRUE, na = "")
