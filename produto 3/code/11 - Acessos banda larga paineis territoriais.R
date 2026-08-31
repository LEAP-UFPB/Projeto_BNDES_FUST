#!/usr/bin/env Rscript

# Testes territoriais para o estoque de acessos de banda larga fixa da Anatel.
#
# A geografia do tratamento e definida exclusivamente pela presenca observada
# na Anatel em dezembro de 2023, antes das principais coortes FUST:
#   1. CNPJ-raiz x municipio: a coorte do CNPJ e atribuida aos municipios em
#      que a empresa tinha acessos em dezembro de 2023;
#   2. municipio: a primeira coorte entre os CNPJs FUST presentes no municipio.
# No segundo desenho, o outcome soma os acessos de todas as prestadoras.

suppressPackageStartupMessages({
  library(data.table)
  library(did)
  library(ggplot2)
  library(openxlsx)
})

args <- commandArgs(FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) stop("Execute este arquivo com Rscript.")
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

years <- 2020:2025
baseline_year <- 2023L
figure_file <- file.path(
  output_dir, "Event study - CS - Log-acessos-banda-larga - Paineis territoriais.png"
)
results_file <- file.path(
  output_dir, "Resultados - Acessos banda larga - Paineis territoriais.xlsx"
)
per_capita_figure_file <- file.path(
  output_dir,
  "Event study - CS - Acessos-banda-larga-per-capita - Paineis territoriais.png"
)
population_file <- file.path(data_dir, "populacao_municipal_censo_2022_ibge.csv")

clean_root <- function(x) {
  z <- gsub("\\D", "", as.character(x))
  short <- !is.na(z) & nchar(z) < 8L
  z[short] <- sprintf("%08d", suppressWarnings(as.integer(z[short])))
  substr(z, 1L, 8L)
}

clean_ibge <- function(x) {
  z <- gsub("\\D", "", as.character(x))
  z[nchar(z) == 6L] <- paste0("0", z[nchar(z) == 6L])
  z[!grepl("^[0-9]{7}$", z)] <- NA_character_
  z
}

source_files <- c(
  `2020` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2019_2020_Colunas.csv"),
  `2021` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2021_Colunas.csv"),
  `2022` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2022_Colunas.csv"),
  `2023` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2023_Colunas.csv"),
  `2024` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2024_Colunas.csv"),
  `2025` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2025_Colunas.csv")
)
if (any(!file.exists(source_files))) {
  stop("Faltam arquivos anuais da Anatel.")
}

read_root_city <- function(yr) {
  f <- source_files[[as.character(yr)]]
  header <- names(fread(f, nrows = 0L, encoding = "UTF-8"))
  december <- paste0(yr, "-12")
  needed <- c("CNPJ", "Código IBGE Município", december)
  if (!all(needed %in% header)) {
    stop("Colunas ausentes em ", basename(f), ": ",
         paste(setdiff(needed, header), collapse = ", "))
  }
  d <- fread(
    f, select = needed,
    colClasses = list(character = c("CNPJ", "Código IBGE Município")),
    encoding = "UTF-8", showProgress = FALSE
  )
  setnames(d, c("cnpj", "ibge", "acessos"))
  d[, `:=`(
    cnpj_raiz = clean_root(cnpj),
    ibge = clean_ibge(ibge),
    acessos = suppressWarnings(as.numeric(acessos))
  )]
  d <- d[
    grepl("^[0-9]{8}$", cnpj_raiz) & !is.na(ibge) &
      is.finite(acessos) & acessos >= 0
  ]
  d[, .(acessos = sum(acessos)), by = .(cnpj_raiz, ibge)]
}

# Todas as recebedoras conhecidas entram na definicao de tratamento, nao apenas
# as empresas que sobreviveram ao matching principal da RAIS.
covars <- fread(
  file.path(data_dir, "produto3_covariaveis_matching.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
all_fust <- unique(covars[
  treat == 1L & is.finite(g_fust),
  .(cnpj_raiz = clean_root(cnpj_raiz), g = as.integer(g_fust))
])
if (all_fust[, anyDuplicated(cnpj_raiz)]) {
  stop("Ha CNPJs FUST com mais de uma primeira coorte.")
}
fust <- all_fust[g %in% 2024:2025]
prior_fust_roots <- all_fust[g < 2024L, cnpj_raiz]

# A pegada territorial e fixada em dezembro de 2023 para nao selecionar
# municipios com base em expansao ocorrida depois do tratamento.
baseline <- read_root_city(baseline_year)[acessos > 0]
root_city_units <- unique(baseline[, .(cnpj_raiz, ibge)])
root_city_units <- root_city_units[!cnpj_raiz %chin% prior_fust_roots]
root_city_units <- merge(root_city_units, fust, by = "cnpj_raiz", all.x = TRUE)
root_city_units[is.na(g), g := 0L]
setorder(root_city_units, cnpj_raiz, ibge)
root_city_units[, id_num := .I]

prior_fust_cities <- unique(baseline[
  cnpj_raiz %chin% prior_fust_roots, ibge
])
city_units <- unique(baseline[!ibge %chin% prior_fust_cities, .(ibge)])
city_treatment <- merge(
  baseline[, .(cnpj_raiz, ibge)], fust, by = "cnpj_raiz", all = FALSE
)[, .(g = min(g)), by = ibge]
city_units <- merge(city_units, city_treatment, by = "ibge", all.x = TRUE)
city_units[is.na(g), g := 0L]
setorder(city_units, ibge)
city_units[, id_num := .I]

root_city_keys <- root_city_units[, .(cnpj_raiz, ibge)]
annual_root_city <- vector("list", length(years))
annual_city <- vector("list", length(years))
names(annual_root_city) <- names(annual_city) <- as.character(years)

for (yr in years) {
  d <- read_root_city(yr)
  annual_city[[as.character(yr)]] <- d[
    ibge %chin% city_units$ibge,
    .(acessos = sum(acessos)), by = ibge
  ][, ano := yr]
  annual_root_city[[as.character(yr)]] <- merge(
    d, root_city_keys, by = c("cnpj_raiz", "ibge"), all = FALSE
  )[, ano := yr]
  message("Anatel ", yr, ": agregacao territorial concluida.")
}

root_city_outcome <- rbindlist(annual_root_city)
city_outcome <- rbindlist(annual_city)

root_city_panel <- merge(
  root_city_units[, .(id_num, cnpj_raiz, ibge, g)][
    , .(ano = years), by = .(id_num, cnpj_raiz, ibge, g)
  ],
  root_city_outcome, by = c("cnpj_raiz", "ibge", "ano"), all.x = TRUE
)
root_city_panel[is.na(acessos), acessos := 0]
root_city_panel[, log_acessos := log1p(acessos)]
setorder(root_city_panel, id_num, ano)

city_panel <- merge(
  city_units[, .(id_num, ibge, g)][
    , .(ano = years), by = .(id_num, ibge, g)
  ],
  city_outcome, by = c("ibge", "ano"), all.x = TRUE
)
city_panel[is.na(acessos), acessos := 0]
city_panel[, log_acessos := log1p(acessos)]
setorder(city_panel, id_num, ano)

# Denominador fixo do Censo 2022. Isso evita misturar as estimativas municipais
# anteriores ao Censo com a nova serie de estimativas iniciada em 2024.
population_url <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/agregados/4709/periodos/2022/",
  "variaveis/93?localidades=N6[all]"
)
if (!file.exists(population_file)) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("O pacote jsonlite e necessario para baixar a populacao do IBGE.")
  }
  population_json <- jsonlite::fromJSON(population_url, simplifyVector = FALSE)
  population_series <- population_json[[1]]$resultados[[1]]$series
  population <- rbindlist(lapply(population_series, function(z) data.table(
    ibge = as.character(z$localidade$id),
    municipio = as.character(z$localidade$nome),
    populacao_2022 = suppressWarnings(as.numeric(z$serie[["2022"]]))
  )))
  fwrite(population, population_file, sep = ";", bom = TRUE)
  message("Populacao municipal baixada do SIDRA/IBGE.")
} else {
  population <- fread(
    population_file, sep = ";", colClasses = c(ibge = "character")
  )
}
population <- population[is.finite(populacao_2022) & populacao_2022 > 0]
if (population[, anyDuplicated(ibge)]) stop("Codigo IBGE duplicado na populacao.")

root_city_panel <- merge(
  root_city_panel, population[, .(ibge, populacao_2022)],
  by = "ibge", all.x = TRUE
)
city_panel <- merge(
  city_panel, population[, .(ibge, populacao_2022)],
  by = "ibge", all.x = TRUE
)
if (root_city_panel[, any(!is.finite(populacao_2022))] ||
    city_panel[, any(!is.finite(populacao_2022))]) {
  stop("Ha municipios do painel sem populacao no Censo 2022.")
}
root_city_panel[, acessos_por_100_hab := 100 * acessos / populacao_2022]
city_panel[, acessos_por_100_hab := 100 * acessos / populacao_2022]
setorder(root_city_panel, id_num, ano)
setorder(city_panel, id_num, ano)

estimate_cs <- function(
    d, label, yname, outcome_label, cluster_var = NULL) {
  fit <- att_gt(
    yname = yname, tname = "ano", idname = "id_num", gname = "g",
    data = as.data.frame(d), xformla = ~1,
    control_group = "nevertreated", base_period = "universal",
    panel = TRUE, allow_unbalanced_panel = FALSE, est_method = "dr",
    bstrap = FALSE, cband = FALSE, clustervars = cluster_var,
    alp = .05, print_details = FALSE
  )
  es <- aggte(
    fit, type = "dynamic", min_e = -5, max_e = 1, na.rm = TRUE,
    bstrap = FALSE, cband = FALSE, alp = .05
  )
  simple <- aggte(
    fit, type = "simple", na.rm = TRUE,
    bstrap = FALSE, cband = FALSE, alp = .05
  )
  dynamic <- data.table(
    painel = label,
    tempo_relativo = as.integer(es$egt),
    att = as.numeric(es$att.egt),
    erro_padrao = as.numeric(es$se.egt)
  )
  dynamic[tempo_relativo == -1L, `:=`(att = 0, erro_padrao = 0)]
  dynamic[, `:=`(
    ic95_inferior = att - qnorm(.975) * erro_padrao,
    ic95_superior = att + qnorm(.975) * erro_padrao
  )]
  overall <- as.numeric(simple$overall.att)
  overall_se <- as.numeric(simple$overall.se)
  summary <- data.table(
    painel = label,
    outcome = outcome_label,
    att_agregado = overall,
    erro_padrao = overall_se,
    ic95_inferior = overall - qnorm(.975) * overall_se,
    ic95_superior = overall + qnorm(.975) * overall_se,
    p_valor = 2 * pnorm(-abs(overall / overall_se)),
    p_tendencias_pre = as.numeric(fit$Wpval),
    unidades_tratadas = uniqueN(d[g > 0, id_num]),
    unidades_controle = uniqueN(d[g == 0, id_num])
  )
  list(summary = summary, dynamic = dynamic, fit = fit)
}

label_root_city <- "CNPJ-raiz x municipio"
label_city <- "Municipio"
est_root_city <- estimate_cs(
  root_city_panel, label_root_city,
  yname = "log_acessos", outcome_label = "log(1 + acessos em dezembro)",
  cluster_var = "cnpj_raiz"
)
message("Estimado: ", label_root_city)
est_city <- estimate_cs(
  city_panel, label_city,
  yname = "log_acessos", outcome_label = "log(1 + acessos em dezembro)"
)
message("Estimado: ", label_city)

est_root_city_pc <- estimate_cs(
  root_city_panel, label_root_city,
  yname = "acessos_por_100_hab",
  outcome_label = "Acessos por 100 habitantes (populacao do Censo 2022)",
  cluster_var = "cnpj_raiz"
)
message("Estimado per capita: ", label_root_city)
est_city_pc <- estimate_cs(
  city_panel, label_city,
  yname = "acessos_por_100_hab",
  outcome_label = "Acessos por 100 habitantes (populacao do Censo 2022)"
)
message("Estimado per capita: ", label_city)

summary_results <- rbindlist(list(est_root_city$summary, est_city$summary))
summary_results[, diagnostico_pre := fifelse(
  p_tendencias_pre < .05,
  "Rejeita tendencias paralelas a 5%; ATT nao deve ser interpretado causalmente",
  "Nao rejeita tendencias paralelas a 5%"
)]
dynamic_results <- rbindlist(list(est_root_city$dynamic, est_city$dynamic))
dynamic_results[, painel := factor(
  painel, levels = c(label_root_city, label_city)
)]

per_capita_summary <- rbindlist(list(
  est_root_city_pc$summary, est_city_pc$summary
))
per_capita_summary[, diagnostico_pre := fifelse(
  p_tendencias_pre < .05,
  "Rejeita tendencias paralelas a 5%; ATT nao deve ser interpretado causalmente",
  "Nao rejeita tendencias paralelas a 5%"
)]
per_capita_dynamic <- rbindlist(list(
  est_root_city_pc$dynamic, est_city_pc$dynamic
))
per_capita_dynamic[, painel := factor(
  painel, levels = c(label_root_city, label_city)
)]

sample_audit <- rbindlist(list(
  root_city_units[, .(
    painel = label_root_city, coorte = g,
    unidades = .N, cnpjs = uniqueN(cnpj_raiz), municipios = uniqueN(ibge)
  ), by = g][, g := NULL],
  city_units[, .(
    painel = label_city, coorte = g,
    unidades = .N, cnpjs = NA_integer_, municipios = uniqueN(ibge)
  ), by = g][, g := NULL]
), fill = TRUE)
setorder(sample_audit, painel, coorte)

p <- ggplot(dynamic_results, aes(tempo_relativo, att)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .35) +
  geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
  geom_errorbar(
    aes(ymin = ic95_inferior, ymax = ic95_superior),
    width = .07, linewidth = .5, color = cor_azul
  ) +
  geom_point(size = 2, color = cor_azul) +
  facet_wrap(~painel, nrow = 1) +
  scale_x_continuous(breaks = -5:1) +
  labs(x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
  tema_bndes(legend_position = "none", base_size = 11)
ggsave(figure_file, p, width = 10.5, height = 5.2, dpi = 300, bg = cor_fundo)

p_per_capita <- ggplot(per_capita_dynamic, aes(tempo_relativo, att)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .35) +
  geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
  geom_errorbar(
    aes(ymin = ic95_inferior, ymax = ic95_superior),
    width = .07, linewidth = .5, color = cor_verde
  ) +
  geom_point(size = 2, color = cor_verde) +
  facet_wrap(~painel, nrow = 1, scales = "free_y") +
  scale_x_continuous(breaks = -5:1) +
  labs(
    x = "Tempo relativo ao primeiro apoio FUST",
    y = "ATT (acessos por 100 habitantes)"
  ) +
  tema_bndes(legend_position = "none", base_size = 11)
ggsave(
  per_capita_figure_file, p_per_capita,
  width = 10.5, height = 5.2, dpi = 300, bg = cor_fundo
)

definitions <- data.table(
  item = c(
    "Pegada territorial", "Painel CNPJ-municipio", "Painel municipio",
    "Tratamento municipal", "Outcome", "Outcome per capita", "Populacao",
    "Periodo", "Estimador", "Inferencia"
  ),
  definicao = c(
    "CNPJ-raiz e codigo IBGE com acessos positivos na Anatel em dezembro de 2023",
    "A coorte do FUST e atribuida a cada municipio da pegada Anatel 2023 do CNPJ",
    "A coorte e o primeiro apoio FUST entre empresas da pegada Anatel 2023 do municipio",
    "Tratado se pelo menos uma empresa observada no municipio recebeu FUST",
    "log(1 + acessos em dezembro); por empresa-municipio no primeiro painel e total municipal no segundo",
    "Acessos em dezembro por 100 habitantes, em nivel",
    paste0("Populacao municipal fixa do Censo 2022, SIDRA/IBGE tabela 4709, variavel 93; ", population_url),
    "2020 a 2025; coortes 2024 e 2025; municipios ja expostos em 2023 excluidos",
    "Callaway-Sant'Anna, controles nunca tratados, DR, periodo-base universal",
    "Intervalos pontuais de 95%, sem bootstrap; painel CNPJ-municipio agrupado por CNPJ"
  )
)

wb <- createWorkbook()
addWorksheet(wb, "ATT agregado")
writeDataTable(wb, "ATT agregado", summary_results)
addWorksheet(wb, "Event study")
writeDataTable(wb, "Event study", dynamic_results)
addWorksheet(wb, "ATT per capita")
writeDataTable(wb, "ATT per capita", per_capita_summary)
addWorksheet(wb, "Event study per capita")
writeDataTable(wb, "Event study per capita", per_capita_dynamic)
addWorksheet(wb, "Amostras")
writeDataTable(wb, "Amostras", sample_audit)
addWorksheet(wb, "Unidades CNPJ municipio")
writeDataTable(wb, "Unidades CNPJ municipio", root_city_units)
addWorksheet(wb, "Unidades municipio")
writeDataTable(wb, "Unidades municipio", city_units)
addWorksheet(wb, "Populacao IBGE 2022")
writeDataTable(wb, "Populacao IBGE 2022", population)
addWorksheet(wb, "Definicoes")
writeDataTable(wb, "Definicoes", definitions)
for (sheet in names(wb)) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:12, widths = "auto")
}
saveWorkbook(wb, results_file, overwrite = TRUE)

print(summary_results)
print(per_capita_summary)
print(sample_audit)
cat("\nFigura: ", figure_file, "\n", sep = "")
cat("Figura per capita: ", per_capita_figure_file, "\n", sep = "")
cat("Resultados: ", results_file, "\n", sep = "")
