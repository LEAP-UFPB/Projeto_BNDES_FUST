#!/usr/bin/env Rscript

# Robustez geral: inferência, controles pré-tratamento × ano, Conley e placebo.

suppressPackageStartupMessages({
  library(data.table)
  library(did)
  library(fixest)
  library(ggplot2)
  library(ggtext)
  library(sf)
})

set.seed(3854035)
args <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

matching_covars <- c(
  "idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores",
  "idade_media_trabalhadores", "prop_homens", "prop_pretos"
)

units <- fread(file.path(data_dir, "produto3_unidades_matching.csv"), sep = ";",
               colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
outcomes <- fread(file.path(data_dir, "produto3_outcomes_rais.csv"), sep = ";",
                  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
units[, cnpj_raiz := clean_root(cnpj_raiz)]
outcomes[, cnpj_raiz := clean_root(cnpj_raiz)]
outcomes <- unique(outcomes[ano >= 2020L & ano <= 2025L], by = c("cnpj_raiz", "ano"))

# Empresas eventualmente tratadas prevalecem sobre linhas em que serviram como
# controle ainda não tratado de outra coorte.
setorder(units, cnpj_raiz, -treat)
u <- unique(units[, c(
  "cnpj_raiz", "treat", "coorte_match", "uf_principal", "municipio_principal",
  matching_covars
), with = FALSE], by = "cnpj_raiz")
u[, g := fifelse(treat == 1L, as.numeric(coorte_match), 0)]
u[, municipio_key := sprintf("%06d", suppressWarnings(as.integer(municipio_principal)))]

# Centroides municipais para a matriz espacial de Conley.
municipios <- st_read(
  file.path(data_dir, "mapas", "BR_Municipios_2021.shp"),
  quiet = TRUE, stringsAsFactors = FALSE
)
municipios <- st_transform(municipios, 4326)
municipios$geometry <- suppressWarnings(st_point_on_surface(municipios$geometry))
xy <- st_coordinates(municipios)
mun_coords <- data.table(
  municipio_key = substr(as.character(municipios$CD_MUN), 1, 6),
  longitude = xy[, 1], latitude = xy[, 2]
)
u <- merge(u, mun_coords, by = "municipio_key", all.x = TRUE)
if (u[, any(!is.finite(latitude) | !is.finite(longitude))]) {
  stop("Há municípios da amostra sem coordenadas para a correção de Conley.")
}

panel <- merge(outcomes, u, by = "cnpj_raiz", all = FALSE)
panel[, id := as.integer(factor(cnpj_raiz))]

cs_att <- function(d, outcome0, label0, placebo = FALSE) {
  z <- copy(d)
  if (placebo) {
    z <- z[ano <= 2023L]
    z[treat == 1L, g := g - 2L]
  }
  z <- z[is.finite(get(outcome0))]
  setnames(z, outcome0, "y")
  fit <- att_gt(
    yname = "y", tname = "ano", idname = "id", gname = "g",
    data = as.data.frame(z), xformla = ~1,
    control_group = "nevertreated", base_period = "universal",
    panel = TRUE, allow_unbalanced_panel = TRUE, est_method = "dr",
    bstrap = FALSE, cband = FALSE, alp = .05, print_details = FALSE
  )
  es <- aggte(
    fit, type = "dynamic", min_e = if (placebo) -2 else -4, max_e = 1,
    na.rm = TRUE, bstrap = FALSE, cband = FALSE, alp = .05
  )
  data.table(
    modelo = label0, att = as.numeric(es$overall.att),
    se = as.numeric(es$overall.se)
  )
}

twfe_att <- function(d, outcome0, label0,
                     vcov_request = ~municipio_principal, controls = FALSE) {
  z <- copy(d)[is.finite(get(outcome0))]
  z[, evt := pmax(-4L, pmin(1L, fifelse(treat == 1L, ano - g, -1L)))]
  control_terms <- if (controls) {
    paste0("i(ano,", matching_covars, ",ref=2020)", collapse = "+")
  } else NULL
  rhs <- paste(c("i(evt,treat,ref=-1)", control_terms), collapse = "+")
  fml <- as.formula(paste0(outcome0, "~", rhs, "|cnpj_raiz+ano"))
  fit <- feols(
    fml, data = z, vcov = vcov_request,
    warn = FALSE, notes = FALSE, fixef.rm = "none"
  )
  ix <- grep("evt::(0|1):treat", names(coef(fit)))
  if (!length(ix)) stop("TWFE sem coeficientes pós-tratamento para ", outcome0)
  b <- coef(fit)[ix]
  V <- vcov(fit)[ix, ix, drop = FALSE]
  w <- rep(1 / length(ix), length(ix))
  data.table(
    modelo = label0,
    att = sum(w * b),
    se = sqrt(drop(t(w) %*% V %*% w))
  )
}

model_order <- c(
  "CS-Base",
  "Cluster: município",
  "Cluster: UF",
  "Conley: 200 km",
  "Controles pré-FUST × ano",
  "Placebo: tratamento 2 anos antes"
)

results <- rbindlist(lapply(seq_len(nrow(outcomes_produto3)), function(i) {
  outcome0 <- outcomes_produto3$outcome[i]
  rbindlist(list(
    cs_att(panel, outcome0, model_order[1]),
    twfe_att(panel, outcome0, model_order[2], ~municipio_principal),
    twfe_att(panel, outcome0, model_order[3], ~uf_principal),
    twfe_att(panel, outcome0, model_order[4],
             vcov_conley(lat = "latitude", lon = "longitude", cutoff = 200)),
    twfe_att(panel, outcome0, model_order[5], ~municipio_principal, controls = TRUE),
    cs_att(panel, outcome0, model_order[6], placebo = TRUE)
  ))[, outcome := outcome0]
}), fill = TRUE)
results[, `:=`(
  low90 = att - qnorm(.95) * se, high90 = att + qnorm(.95) * se,
  low95 = att - qnorm(.975) * se, high95 = att + qnorm(.975) * se,
  modelo_plot = factor(modelo, levels = rev(model_order))
)]

for (i in seq_len(nrow(outcomes_produto3))) {
  dd <- results[outcome == outcomes_produto3$outcome[i]]
  p <- ggplot(dd, aes(att, modelo_plot)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey45") +
    geom_errorbarh(aes(xmin = low95, xmax = high95), height = 0,
                   linewidth = .55, color = cor_azul) +
    geom_errorbarh(aes(xmin = low90, xmax = high90), height = 0,
                   linewidth = 1.5, color = cor_azul, alpha = .75) +
    geom_point(size = 2.7, color = cor_azul) +
    labs(x = "ATT", y = NULL) +
    tema_bndes(legend_position = "none", base_size = 11) +
    theme(panel.grid.major.y = element_blank())
  ggsave(file.path(output_dir, paste0("Robustez - ", outcomes_produto3$safe[i], ".png")),
         p, width = 8.5, height = 5.5, dpi = 300, bg = cor_fundo)
}

print(results)
