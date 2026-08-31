#!/usr/bin/env Rscript

# Robustez a estimadores alternativos: TWFE, Gardner/did2s, BJS e SDID.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(did2s)
  library(synthdid)
  library(ggplot2)
})

set.seed(3854035)
args <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

units <- fread(file.path(data_dir, "produto3_unidades_matching.csv"), sep = ";",
               colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
outcomes <- fread(file.path(data_dir, "produto3_outcomes_rais.csv"), sep = ";",
                  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
units[, cnpj_raiz := clean_root(cnpj_raiz)]
outcomes[, cnpj_raiz := clean_root(cnpj_raiz)]
outcomes <- unique(outcomes[ano >= 2020L & ano <= 2025L], by = c("cnpj_raiz", "ano"))
setorder(units, cnpj_raiz, -treat)
u <- unique(units[, .(cnpj_raiz, treat, coorte_match, municipio_principal)], by = "cnpj_raiz")
u[, g := fifelse(treat == 1L, as.numeric(coorte_match), 0)]
panel <- merge(outcomes, u, by = "cnpj_raiz", all = FALSE)

twfe_event <- function(d, outcome0) {
  z <- copy(d)[is.finite(get(outcome0))]
  z[, evt := pmax(-4L, pmin(1L, fifelse(treat == 1L, ano - g, -1L)))]
  fit <- feols(
    as.formula(paste0(outcome0, "~i(evt,treat,ref=-1)|cnpj_raiz+ano")),
    data = z, cluster = ~municipio_principal, warn = FALSE, notes = FALSE
  )
  ix <- grep("evt::(-4|-3|-2|0|1):treat", names(coef(fit)))
  ans <- data.table(
    event_time = as.integer(sub("evt::(-?[0-9]+):treat.*", "\\1", names(coef(fit))[ix])),
    att = as.numeric(coef(fit)[ix]),
    se = sqrt(diag(vcov(fit)[ix, ix, drop = FALSE]))
  )
  rbind(ans, data.table(event_time = -1L, att = 0, se = 0))[order(event_time)]
}

sdid_curve_once <- function(d, outcome0) {
  curves <- list()
  for (g0 in sort(unique(d[g > 0, g]))) {
    z <- copy(d[g %in% c(0, g0), .(cnpj_raiz, ano, y = get(outcome0), g)])
    complete <- z[, all(is.finite(y)), by = cnpj_raiz][V1 == TRUE, cnpj_raiz]
    z <- z[cnpj_raiz %in% complete]
    z[, D := as.integer(g == g0 & ano >= g0)]
    pm <- tryCatch(
      panel.matrices(as.data.frame(z), unit = "cnpj_raiz", time = "ano", outcome = "y", treatment = "D"),
      error = function(e) NULL
    )
    if (is.null(pm) || pm$N0 < 1L) next
    est <- tryCatch(synthdid_estimate(pm$Y, pm$N0, pm$T0), error = function(e) NULL)
    if (is.null(est)) next
    setup <- attr(est, "setup")
    ww <- attr(est, "weights")
    Y <- setup$Y
    N0 <- setup$N0
    T0 <- setup$T0
    N1 <- nrow(Y) - N0
    gap <- as.numeric(t(c(-ww$omega, rep(1 / N1, N1))) %*% Y)
    base <- as.numeric(gap[seq_len(T0)] %*% ww$lambda)
    curves[[as.character(g0)]] <- data.table(
      event_time = as.integer(colnames(Y)) - g0,
      att = gap - base, weight = N1
    )
  }
  rbindlist(curves)[, .(att = weighted.mean(att, weight)), by = event_time][
    event_time >= -4L & event_time <= 1L
  ][, .(event_time, att)]
}

sdid_event <- function(d, outcome0, reps = 30L) {
  point <- sdid_curve_once(d, outcome0)
  point <- rbind(point[event_time != -1L], data.table(event_time = -1L, att = 0))[order(event_time)]
  ids <- unique(d$cnpj_raiz)
  pieces <- split(d, d$cnpj_raiz)
  boot <- matrix(NA_real_, reps, nrow(point))
  set.seed(3854035)
  for (b in seq_len(reps)) {
    take <- sample(ids, length(ids), replace = TRUE)
    db <- rbindlist(lapply(seq_along(take), function(i) {
      q <- copy(pieces[[take[i]]])
      q[, cnpj_raiz := sprintf("B%05d", i)]
      q
    }))
    eb <- tryCatch(sdid_curve_once(db, outcome0), error = function(e) NULL)
    if (!is.null(eb)) boot[b, ] <- eb$att[match(point$event_time, eb$event_time)]
  }
  point[, se := apply(boot, 2, sd, na.rm = TRUE)]
  point[event_time == -1L, se := 0]
  point
}

estimator_levels <- c("TWFE", "Gardner (did2s)", "BJS (imputação)", "SDID")
all_results <- list()
for (i in seq_len(nrow(outcomes_produto3))) {
  outcome0 <- outcomes_produto3$outcome[i]
  complete_ids <- panel[, .(
    ok = uniqueN(ano) == 6L && all(is.finite(get(outcome0)))
  ), by = cnpj_raiz][ok == TRUE, cnpj_raiz]
  z <- copy(panel[cnpj_raiz %in% complete_ids])
  z[, id_alt := as.integer(factor(cnpj_raiz))]

  all_results[[paste(outcome0, "twfe")]] <- twfe_event(z, outcome0)[,
    `:=`(estimador = "TWFE", outcome = outcome0)]
  for (est0 in c("did2s", "impute")) {
    ee <- event_study(
      as.data.frame(z), yname = outcome0, idname = "id_alt",
      gname = "g", tname = "ano", estimator = est0
    )
    ee <- as.data.table(ee)[term >= -4 & term <= 1,
      .(event_time = as.integer(term), att = estimate, se = std.error)]
    ee <- rbind(ee[event_time != -1L], data.table(event_time = -1L, att = 0, se = 0))[order(event_time)]
    ee[, `:=`(
      estimador = if (est0 == "did2s") "Gardner (did2s)" else "BJS (imputação)",
      outcome = outcome0
    )]
    all_results[[paste(outcome0, est0)]] <- ee
  }
  all_results[[paste(outcome0, "sdid")]] <- sdid_event(z, outcome0)[,
    `:=`(estimador = "SDID", outcome = outcome0)]
}

results <- rbindlist(all_results, fill = TRUE)
results[, `:=`(
  low = att - qnorm(.975) * se,
  high = att + qnorm(.975) * se,
  estimador = factor(estimador, levels = estimator_levels)
)]
pd <- position_dodge(width = .18)
for (i in seq_len(nrow(outcomes_produto3))) {
  dd <- results[outcome == outcomes_produto3$outcome[i]]
  p <- ggplot(dd, aes(event_time, att, color = estimador, group = estimador)) +
    geom_hline(yintercept = 0, color = "grey55", linewidth = .35) +
    geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
    geom_errorbar(aes(ymin = low, ymax = high), width = .08, position = pd, linewidth = .45) +
    geom_point(position = pd, size = 1.8) +
    scale_x_continuous(breaks = -4:1) +
    scale_color_manual(values = paleta_bndes[1:4]) +
    labs(x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
    tema_bndes(legend_position = "bottom", base_size = 11)
  ggsave(file.path(output_dir, paste0("Robustez - Estimadores - ", outcomes_produto3$safe[i], ".png")),
         p, width = 8.5, height = 5.2, dpi = 300, bg = cor_fundo)
}

print(results)
