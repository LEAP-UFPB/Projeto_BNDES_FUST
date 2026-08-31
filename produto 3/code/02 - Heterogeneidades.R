#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(did)
  library(ggplot2)
  library(ggtext)
  library(readxl)
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
covars <- fread(file.path(data_dir, "produto3_covariaveis_matching.csv"), sep = ";",
                colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
classes <- fread(file.path(data_dir, "produto3_modalidade_fust.csv"), sep = ";",
                 colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
outcomes <- fread(file.path(data_dir, "produto3_outcomes_rais.csv"), sep = ";",
                  colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
for (z in c("units", "covars", "classes", "outcomes")) {
  get(z)[, cnpj_raiz := clean_root(cnpj_raiz)]
}
outcomes <- unique(outcomes[ano >= 2020L & ano <= 2025L], by = c("cnpj_raiz", "ano"))
covars[, division := substr(as.character(as.integer(cnae_classe_principal)), 1, 2)]

matching_covars <- c(
  "idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores",
  "idade_media_trabalhadores", "prop_homens", "prop_pretos"
)

# Filiais/unidades são medidas no ano anterior ao tratamento.
firm_panel <- as.data.table(read_excel(
  file.path(data_dir, "Painel_CNPJ_FUST_RAIS_2018_2024.xlsx"),
  sheet = "painel_cnpj_ano"
))
firm_panel[, cnpj_raiz := clean_root(cnpj_raiz)]
branch_traits <- firm_panel[ano %in% c(2023L, 2024L),
  .(cnpj_raiz, coorte_match = ano + 1L, total_firmas)]

# Porte e número de subcréditos vêm da base operacional do BNDES. Na modalidade
# indireta, cada linha da base corresponde a um subcrédito/operação financiada.
norm_text <- function(x) {
  x <- iconv(as.character(x), to = "ASCII//TRANSLIT")
  toupper(gsub("[^A-Z0-9]+", "", x))
}
wb_fust <- list.files(data_dir, pattern = "com_num_OPE_subcr", full.names = TRUE)[1]
if (is.na(wb_fust) || !file.exists(wb_fust)) stop("Base operacional do FUST não encontrada")
bndes_parts <- lapply(1:2, function(s) {
  z <- as.data.table(read_excel(wb_fust, sheet = s, skip = if (s == 1L) 4L else 5L))
  z[, .(key = norm_text(Cliente), porte_bndes = norm_text(`Porte do cliente`))]
})
bndes_key <- rbindlist(bndes_parts)[, .(
  porte_bndes = names(sort(table(porte_bndes), decreasing = TRUE))[1],
  n_subcreditos = .N
), by = key]
bndes_traits <- merge(
  unique(classes[grepl("^[0-9]{8}$", cnpj_raiz), .(cnpj_raiz, key)]),
  bndes_key,
  by = "key"
)[, .(
  porte_bndes = names(sort(table(porte_bndes), decreasing = TRUE))[1],
  n_subcreditos = max(n_subcreditos)
), by = cnpj_raiz]

units <- merge(units, branch_traits, by = c("cnpj_raiz", "coorte_match"), all.x = TRUE)
units <- merge(units, bndes_traits, by = "cnpj_raiz", all.x = TRUE)

age_cut <- as.integer(median(units[treat == 1L, idade_firma_pre], na.rm = TRUE))
age_groups <- c(paste0("Até ", age_cut, " anos"), paste0("Maior que ", age_cut, " anos"))
skill_cut <- median(units[treat == 1L, prop_superior], na.rm = TRUE)
groups <- data.table(
  dimensao = c(rep("Modalidade", 2), rep("Porte", 3), rep("Setor", 2),
               rep("Região", 3), rep("Idade da firma", 2), rep("Qualificação", 2),
               rep("Filiais", 2), rep("Subcréditos", 2)),
  grupo = c("Direta", "Indireta", "Micro e pequena", "Média", "Grande",
            "Telecom por fio", "Outras telecom", "Sul", "Sudeste", "Outras regiões",
            age_groups, "Baixa qualificação", "Alta qualificação",
            "Uma unidade", "Mais de uma unidade",
            "Um subcrédito", "Mais de um subcrédito")
)

eligible_ids <- function(grupo0, g0, treated = TRUE) {
  z <- units[treat == as.integer(treated) & coorte_match == g0]
  if (grupo0 %in% c("Direta", "Indireta") && treated) {
    return(intersect(
      z$cnpj_raiz,
      classes[tipo_operacao == grupo0 & coorte == g0, cnpj_raiz]
    ))
  }
  # Porte BNDES, filiais e subcréditos são atributos das tratadas. Para cada
  # subgrupo, o matching volta a selecionar controles no pool oficial inteiro.
  if (treated && grupo0 == "Micro e pequena") z <- z[porte_bndes %in% c("MICRO", "PEQUENA")]
  if (treated && grupo0 == "Média") z <- z[porte_bndes == "MEDIA"]
  if (treated && grupo0 == "Grande") z <- z[porte_bndes == "GRANDE"]
  if (treated && grupo0 == "Uma unidade") z <- z[total_firmas <= 1]
  if (treated && grupo0 == "Mais de uma unidade") z <- z[total_firmas > 1]
  if (treated && grupo0 == "Um subcrédito") z <- z[n_subcreditos <= 1]
  if (treated && grupo0 == "Mais de um subcrédito") z <- z[n_subcreditos > 1]
  if (!treated && grupo0 %in% c("Micro e pequena", "Média", "Grande",
                                "Uma unidade", "Mais de uma unidade",
                                "Um subcrédito", "Mais de um subcrédito")) {
    return(z$cnpj_raiz)
  }
  if (grupo0 == "Telecom por fio") z <- z[cnae_classe_principal == 6110L]
  if (grupo0 == "Outras telecom") z <- z[cnae_classe_principal == 6190L]
  if (grupo0 == "Sul") z <- z[regiao == "Sul"]
  if (grupo0 == "Sudeste") z <- z[regiao == "Sudeste"]
  if (grupo0 == "Outras regiões") z <- z[!regiao %in% c("Sul", "Sudeste")]
  if (grupo0 == age_groups[1]) z <- z[idade_firma_pre <= age_cut]
  if (grupo0 == age_groups[2]) z <- z[idade_firma_pre > age_cut]
  if (grupo0 == "Baixa qualificação") z <- z[prop_superior <= skill_cut]
  if (grupo0 == "Alta qualificação") z <- z[prop_superior > skill_cut]
  z$cnpj_raiz
}

# Refaz matching 1:1 para cada subgrupo dentro da amostra oficial S4.
matched_parts <- list()
for (grupo0 in groups$grupo) {
  used_controls <- character()
  for (g0 in sort(unique(units$coorte_match))) {
    treated_ids <- eligible_ids(grupo0, g0, TRUE)
    control_ids <- setdiff(eligible_ids(grupo0, g0, FALSE), used_controls)
    d <- unique(covars[g == g0 & cnpj_raiz %in% c(treated_ids, control_ids)], by = "cnpj_raiz")
    d[, treat := as.integer(cnpj_raiz %in% treated_ids)]
    d <- d[complete.cases(d[, c(matching_covars, "division"), with = FALSE])]
    if (!d[, any(treat == 1L)] || !d[, any(treat == 0L)]) next
    fit <- tryCatch(
      matchit(
        reformulate(matching_covars, "treat"), data = d,
        method = "nearest", distance = "mahalanobis", exact = ~division,
        ratio = 1, replace = FALSE, m.order = "farthest"
      ),
      error = function(e) {
        message(sprintf("%s/%d: fallback para propensity score (%s)",
                        grupo0, g0, conditionMessage(e)))
        matchit(
          reformulate(matching_covars, "treat"), data = d,
          method = "nearest", distance = "glm", exact = ~division,
          ratio = 1, replace = FALSE, m.order = "largest"
        )
      }
    )
    m <- as.data.table(match.data(fit, drop.unmatched = TRUE))
    if (!nrow(m[treat == 1L]) || !nrow(m[treat == 0L])) next
    m[, `:=`(grupo = grupo0,
             dimensao = groups[groups[["grupo"]] == grupo0, dimensao][1],
             coorte_match = g0)]
    used_controls <- union(used_controls, m[treat == 0L, cnpj_raiz])
    matched_parts[[paste(grupo0, g0, sep = "|")]] <- m
  }
}
hetero_units <- rbindlist(matched_parts, fill = TRUE)
fwrite(hetero_units, file.path(data_dir, "produto3_unidades_heterogeneidade.csv"),
       sep = ";", bom = TRUE)

estimate_group <- function(grupo0, outcome0) {
  m <- hetero_units[grupo == grupo0]
  uu <- m[, .(
    g = if (any(treat == 1L)) as.numeric(unique(coorte_match[treat == 1L])[1]) else 0
  ), by = cnpj_raiz]
  d <- merge(outcomes[, c("cnpj_raiz", "ano", outcome0), with = FALSE], uu, by = "cnpj_raiz")
  setnames(d, outcome0, "y")
  d <- d[is.finite(y)]
  d[, id := .GRP, by = cnpj_raiz]
  fit <- att_gt(
    yname = "y", tname = "ano", idname = "id", gname = "g",
    data = as.data.frame(d), xformla = ~1, control_group = "nevertreated",
    base_period = "universal", panel = TRUE, allow_unbalanced_panel = TRUE,
    est_method = "dr", bstrap = FALSE, cband = FALSE,
    alp = .05, print_details = FALSE
  )
  es <- aggte(fit, type = "dynamic", min_e = -4, max_e = 1, na.rm = TRUE,
              bstrap = FALSE, cband = FALSE, alp = .05)
  data.table(
    dimensao = groups[groups[["grupo"]] == grupo0, dimensao][1],
    grupo = grupo0, outcome = outcome0,
    att = as.numeric(es$overall.att), se = as.numeric(es$overall.se),
    tratadas = uniqueN(m[treat == 1L, cnpj_raiz]),
    controles = uniqueN(m[treat == 0L, cnpj_raiz])
  )
}

att_table <- rbindlist(lapply(groups$grupo, function(grupo0) {
  rbindlist(lapply(outcomes_produto3$outcome, function(outcome0) {
    tryCatch(estimate_group(grupo0, outcome0), error = function(e) {
      message(sprintf("Falha em %s/%s: %s", grupo0, outcome0, conditionMessage(e)))
      NULL
    })
  }), fill = TRUE)
}), fill = TRUE)
att_table[, `:=`(
  low90 = att - qnorm(.95) * se, high90 = att + qnorm(.95) * se,
  low95 = att - qnorm(.975) * se, high95 = att + qnorm(.975) * se
)]
fwrite(att_table, file.path(data_dir, "produto3_resultados_heterogeneidade.csv"),
       sep = ";", bom = TRUE)

panel_names <- unique(groups$dimensao)
make_heterogeneity_plot <- function(outcome0) {
  dd <- copy(att_table[outcome == outcome0])
  layout <- list()
  pos <- 0
  for (dim0 in panel_names) {
    pos <- pos + 1
    layout[[length(layout) + 1L]] <- data.table(dimensao = dim0, grupo = NA_character_, y = pos, header = TRUE)
    for (gr0 in groups[dimensao == dim0, grupo]) {
      pos <- pos + 1
      layout[[length(layout) + 1L]] <- data.table(dimensao = dim0, grupo = gr0, y = pos, header = FALSE)
    }
    pos <- pos + .35
  }
  lay <- rbindlist(layout)
  dd <- merge(dd, lay[header == FALSE], by = c("dimensao", "grupo"), all.x = TRUE)
  labels_y <- vapply(seq_len(nrow(lay)), function(i) {
    if (lay$header[i]) paste0("<b>", toupper(lay$dimensao[i]), ":</b>")
    else paste0("&nbsp;&nbsp;", lay$grupo[i])
  }, character(1))
  ggplot(dd, aes(att, y)) +
    geom_vline(xintercept = 0, linetype = 2, color = "grey45") +
    geom_errorbarh(aes(xmin = low95, xmax = high95), height = 0,
                   linewidth = .55, color = cor_azul) +
    geom_errorbarh(aes(xmin = low90, xmax = high90), height = 0,
                   linewidth = 1.5, color = cor_azul, alpha = .75) +
    geom_point(size = 2.7, color = cor_azul) +
    scale_y_reverse(breaks = lay$y, labels = labels_y,
                    limits = c(max(lay$y) + .5, min(lay$y) - .5), expand = c(0, 0)) +
    labs(x = "ATT", y = "Amostra") +
    tema_bndes(legend_position = "none", base_size = 11) +
    theme(axis.ticks.y = element_blank(), axis.text.y = element_markdown(size = 9),
          panel.grid.major.y = element_blank())
}

for (i in seq_len(nrow(outcomes_produto3))) {
  fig <- make_heterogeneity_plot(outcomes_produto3$outcome[i])
  ggsave(file.path(output_dir, paste0("Heterogeneidades - ", outcomes_produto3$safe[i], ".png")),
         fig, width = 8.5, height = 10, dpi = 300, bg = cor_fundo)
}

print(att_table[, .(dimensao, grupo, outcome, tratadas, controles)])
