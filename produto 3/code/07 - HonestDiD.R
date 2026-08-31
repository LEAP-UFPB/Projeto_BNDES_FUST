#!/usr/bin/env Rscript

# Sensibilidade a violações de tendências paralelas (Rambachan--Roth).

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(HonestDiD)
  library(ggplot2)
})

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

Mbarvec <- seq(.1, 1, by = .1)
for (i in seq_len(nrow(outcomes_produto3))) {
  outcome0 <- outcomes_produto3$outcome[i]
  z <- copy(panel[is.finite(get(outcome0))])
  z[, evt := pmax(-4L, pmin(1L, fifelse(treat == 1L, ano - g, -1L)))]
  fit <- feols(
    as.formula(paste0(outcome0, "~i(evt,treat,ref=-1)|cnpj_raiz+ano")),
    data = z, cluster = ~municipio_principal, warn = FALSE, notes = FALSE
  )
  wanted <- paste0("evt::", c(-4, -3, -2, 0, 1), ":treat")
  idx <- match(wanted, names(coef(fit)))
  if (anyNA(idx)) stop("HonestDiD sem todos os períodos para ", outcome0)
  beta <- coef(fit)[idx]
  sigma <- vcov(fit)[idx, idx, drop = FALSE]
  lvec <- c(.5, .5)
  sensitivity <- createSensitivityResults_relativeMagnitudes(
    betahat = beta, sigma = sigma, numPrePeriods = 3,
    numPostPeriods = 2, Mbarvec = Mbarvec, l_vec = lvec
  )
  original <- constructOriginalCS(
    betahat = beta, sigma = sigma, numPrePeriods = 3,
    numPostPeriods = 2, l_vec = lvec
  )
  p <- createSensitivityPlot_relativeMagnitudes(sensitivity, original) +
    scale_color_manual(values = c("C-LF" = cor_azul, "Original" = cor_verde)) +
    scale_x_continuous(breaks = c(0, Mbarvec)) +
    labs(x = expression(bar(M)), y = "Intervalo de confiança") +
    tema_bndes(legend_position = "bottom", base_size = 11)
  ggsave(file.path(output_dir, paste0("HonestDiD - ", outcomes_produto3$safe[i], ".png")),
         p, width = 8.5, height = 5.2, dpi = 300, bg = cor_fundo)
}

cat("HonestDiD concluído para os três outcomes.\n")
