args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

# Série SGS 7811 do Banco Central: TR mensal para período iniciado no dia 1º.
# Consulta: https://api.bcb.gov.br/dados/serie/bcdata.sgs.7811/dados
tr_mensal <- tibble::tribble(
  ~ano, ~mes, ~tr_mensal_pct,
  2024L,  1L, 0.0875,
  2024L,  2L, 0.0079,
  2024L,  3L, 0.0331,
  2024L,  4L, 0.1023,
  2024L,  5L, 0.0870,
  2024L,  6L, 0.0365,
  2024L,  7L, 0.0739,
  2024L,  8L, 0.0707,
  2024L,  9L, 0.0675,
  2024L, 10L, 0.0977,
  2024L, 11L, 0.0649,
  2024L, 12L, 0.0822,
  2025L,  1L, 0.1690,
  2025L,  2L, 0.1324,
  2025L,  3L, 0.1092,
  2025L,  4L, 0.1689,
  2025L,  5L, 0.1712,
  2025L,  6L, 0.1699,
  2025L,  7L, 0.1758,
  2025L,  8L, 0.1722,
  2025L,  9L, 0.1742,
  2025L, 10L, 0.1758,
  2025L, 11L, 0.1634,
  2025L, 12L, 0.1742
)

tr_safra <- tr_mensal |>
  group_by(ano) |>
  summarise(
    tr_acumulada_pct = (prod(1 + tr_mensal_pct / 100) - 1) * 100,
    .groups = "drop"
  )

classificar_custo <- function(x) {
  custo <- normalizar_texto(x)
  case_when(
    str_detect(custo, "TAXA FIXA") ~ "Taxa fixa",
    str_detect(custo, "TAXA REFERENCIAL|^TR$") ~ "TR",
    TRUE ~ NA_character_
  )
}

base_juros <- base_2024_2025() |>
  mutate(
    modalidade = as.character(origem),
    tipo_taxa = classificar_custo(custo_financeiro)
  ) |>
  filter(!is.na(tipo_taxa), is.finite(juros_numero)) |>
  left_join(tr_safra, by = "ano") |>
  mutate(
    taxa_informada_pct = juros_numero,
    taxa_ajustada_pct = if_else(
      tipo_taxa == "TR",
      juros_numero + tr_acumulada_pct,
      juros_numero
    )
  )

resumir_taxa <- function(df, variavel, periodo) {
  df |>
    group_by(modalidade, tipo_taxa, {{ periodo }}) |>
    summarise(
      observacoes = n(),
      minimo = min({{ variavel }}),
      primeiro_quartil = quantile({{ variavel }}, 0.25, names = FALSE),
      mediana = median({{ variavel }}),
      media = mean({{ variavel }}),
      terceiro_quartil = quantile({{ variavel }}, 0.75, names = FALSE),
      maximo = max({{ variavel }}),
      .groups = "drop"
    )
}

montar_tabela <- function(df, variavel) {
  anual <- resumir_taxa(df, {{ variavel }}, ano) |>
    mutate(periodo = as.character(ano)) |>
    select(-ano)

  consolidado <- df |>
    mutate(periodo = "2024–2025") |>
    resumir_taxa({{ variavel }}, periodo)

  bind_rows(anual, consolidado) |>
    mutate(
      modalidade = factor(modalidade, levels = c("Direta", "Indireta")),
      tipo_taxa = factor(tipo_taxa, levels = c("TR", "Taxa fixa")),
      periodo = factor(periodo, levels = c("2024", "2025", "2024–2025"))
    ) |>
    arrange(modalidade, tipo_taxa, periodo) |>
    transmute(
      Modalidade = as.character(modalidade),
      `Tipo de taxa` = as.character(tipo_taxa),
      `Ano-safra` = as.character(periodo),
      Observações = observacoes,
      `Mínimo (%)` = minimo,
      `1º quartil (%)` = primeiro_quartil,
      `Mediana (%)` = mediana,
      `Média (%)` = media,
      `3º quartil (%)` = terceiro_quartil,
      `Máximo (%)` = maximo
    )
}

tabela_informada <- montar_tabela(base_juros, taxa_informada_pct)
tabela_ajustada <- montar_tabela(base_juros, taxa_ajustada_pct)

tabela_comparativa <- bind_rows(
  tabela_informada |> mutate(`Valor considerado` = "1. Valor informado"),
  tabela_ajustada |> mutate(`Valor considerado` = "2. Valor ajustado pela TR")
) |>
  mutate(
    .ord_modalidade = match(Modalidade, c("Direta", "Indireta")),
    .ord_tipo = match(`Tipo de taxa`, c("TR", "Taxa fixa")),
    .ord_periodo = match(`Ano-safra`, c("2024", "2025", "2024–2025")),
    .ord_valor = match(
      `Valor considerado`,
      c("1. Valor informado", "2. Valor ajustado pela TR")
    )
  ) |>
  arrange(.ord_modalidade, .ord_tipo, .ord_periodo, .ord_valor) |>
  select(
    Modalidade,
    `Tipo de taxa`,
    `Ano-safra`,
    `Valor considerado`,
    everything(),
    -.ord_modalidade,
    -.ord_tipo,
    -.ord_periodo,
    -.ord_valor
  )

tabela_tr <- tr_mensal |>
  left_join(tr_safra, by = "ano") |>
  transmute(
    `Ano-safra` = ano,
    Mês = mes,
    `TR mensal (%)` = tr_mensal_pct,
    `TR acumulada no ano-safra (%)` = tr_acumulada_pct
  )

arquivo_saida <- file.path(
  output_secao_operacional_dir,
  "1.5 Distribuição por faixa de taxa de juros e prazo total",
  "Tabela 3 - Estatísticas descritivas das taxas de juros - TR e taxa fixa, 2024–2025.xlsx"
)
dir.create(dirname(arquivo_saida), recursive = TRUE, showWarnings = FALSE)

wb <- openxlsx::createWorkbook()
estilo_titulo <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 14, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = cor_azul, halign = "left"
)
estilo_cabecalho <- openxlsx::createStyle(
  fontName = "Arial", textDecoration = "bold", fontColour = "#FFFFFF",
  fgFill = cor_verde, halign = "center", valign = "center", wrapText = TRUE,
  border = "Bottom", borderColour = "#FFFFFF"
)
estilo_percentual <- openxlsx::createStyle(numFmt = "0.0000\"%\"")
estilo_inteiro <- openxlsx::createStyle(numFmt = "#,##0")
estilo_nota <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 10, fontColour = "#404040", wrapText = TRUE
)

escrever_tabela <- function(aba, titulo, dados, nota) {
  openxlsx::addWorksheet(wb, aba, gridLines = FALSE)
  openxlsx::writeData(wb, aba, titulo, startRow = 1)
  openxlsx::mergeCells(wb, aba, cols = 1:ncol(dados), rows = 1)
  openxlsx::addStyle(wb, aba, estilo_titulo, rows = 1, cols = 1:ncol(dados))
  openxlsx::writeData(wb, aba, dados, startRow = 3, withFilter = FALSE)
  openxlsx::addStyle(wb, aba, estilo_cabecalho, rows = 3, cols = 1:ncol(dados))
  linhas <- 4:(3 + nrow(dados))
  coluna_observacoes <- match("Observações", names(dados))
  colunas_percentuais <- which(str_detect(names(dados), "\\(%\\)$"))
  openxlsx::addStyle(
    wb, aba, estilo_inteiro,
    rows = linhas, cols = coluna_observacoes, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, aba, estilo_percentual,
    rows = linhas, cols = colunas_percentuais, gridExpand = TRUE
  )
  linha_nota <- 5 + nrow(dados)
  openxlsx::writeData(wb, aba, nota, startRow = linha_nota)
  openxlsx::mergeCells(wb, aba, cols = 1:ncol(dados), rows = linha_nota)
  openxlsx::addStyle(wb, aba, estilo_nota, rows = linha_nota, cols = 1:ncol(dados))
  openxlsx::setColWidths(wb, aba, cols = 1:3, widths = c(15, 16, 14))
  openxlsx::setColWidths(wb, aba, cols = 4, widths = 26)
  openxlsx::setColWidths(wb, aba, cols = 5:ncol(dados), widths = 18)
  openxlsx::setRowHeights(wb, aba, rows = linha_nota, heights = 45)
  openxlsx::freezePane(wb, aba, firstActiveRow = 4)
}

escrever_tabela(
  "Comparativo",
  "Taxas informadas e ajustadas pela TR do ano-safra",
  tabela_comparativa,
  paste(
    "Cada grupo apresenta primeiro o valor informado e depois o valor ajustado.",
    "Para operações TR: taxa ajustada = spread informado + TR acumulada no ano-safra;",
    "para taxa fixa, os valores informado e ajustado são iguais.",
    "Não há operações com taxa fixa no apoio direto em 2024–2025."
  )
)

openxlsx::addWorksheet(wb, "TR por safra", gridLines = FALSE)
openxlsx::writeData(wb, "TR por safra", "TR mensal e acumulada por ano-safra", startRow = 1)
openxlsx::mergeCells(wb, "TR por safra", cols = 1:4, rows = 1)
openxlsx::addStyle(wb, "TR por safra", estilo_titulo, rows = 1, cols = 1:4)
openxlsx::writeData(wb, "TR por safra", tabela_tr, startRow = 3, withFilter = FALSE)
openxlsx::addStyle(wb, "TR por safra", estilo_cabecalho, rows = 3, cols = 1:4)
openxlsx::addStyle(
  wb, "TR por safra", estilo_percentual,
  rows = 4:(3 + nrow(tabela_tr)), cols = 3:4, gridExpand = TRUE
)
linha_fonte <- 5 + nrow(tabela_tr)
openxlsx::writeData(
  wb, "TR por safra",
  paste(
    "Fonte: Banco Central do Brasil, SGS 7811 (TR para período iniciado no dia 1º).",
    "Acumulado anual = produto de (1 + TR mensal/100), menos 1."
  ),
  startRow = linha_fonte
)
openxlsx::mergeCells(wb, "TR por safra", cols = 1:4, rows = linha_fonte)
openxlsx::addStyle(wb, "TR por safra", estilo_nota, rows = linha_fonte, cols = 1:4)
openxlsx::setColWidths(wb, "TR por safra", cols = 1:4, widths = c(15, 12, 20, 34))
openxlsx::freezePane(wb, "TR por safra", firstActiveRow = 4)

openxlsx::saveWorkbook(wb, arquivo_saida, overwrite = TRUE)
message("Arquivo salvo em: ", arquivo_saida)
message(
  "TR acumulada: ",
  paste0(tr_safra$ano, " = ", sprintf("%.4f%%", tr_safra$tr_acumulada_pct), collapse = "; ")
)
