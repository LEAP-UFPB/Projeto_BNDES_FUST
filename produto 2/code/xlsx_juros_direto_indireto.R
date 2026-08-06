args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

normalizar_custo <- function(x) {
  custo <- normalizar_texto(x)
  case_when(
    str_detect(custo, "TAXA FIXA") ~ "Taxa fixa",
    str_detect(custo, "TAXA REFERENCIAL|^TR$") ~ "Taxa de referência (TR)",
    TRUE ~ NA_character_
  )
}

dados_juros <- base_2024_2025() |>
  mutate(
    origem = factor(origem, levels = c("Direta", "Indireta")),
    tipo_custo = factor(
      normalizar_custo(custo_financeiro),
      levels = c("Taxa de referência (TR)", "Taxa fixa")
    )
  ) |>
  filter(!is.na(tipo_custo)) |>
  resumir(origem, tipo_custo, ano, juros_faixa) |>
  arrange(origem, tipo_custo, ano, juros_faixa) |>
  rename(
    Ano = ano,
    `Faixa de juros` = juros_faixa,
    Subcréditos = contratos,
    `Valor contratado (R$)` = valor_contratado,
    `Valor desembolsado (R$)` = valor_desembolsado,
    `Ticket médio (R$)` = ticket_medio,
    `Juros médio (%)` = juros_medio,
    `Juros médio ponderado (%)` = juros_medio_ponderado
  )

arquivo_saida <- file.path(
  output_secao_operacional_dir,
  "1.5 Distribuição por faixa de taxa de juros e prazo total",
  "Tabela 2 - Distribuição por faixa de taxa de juros - Apoio Indireto, 2024–2025.xlsx"
)
dir.create(dirname(arquivo_saida), recursive = TRUE, showWarnings = FALSE)

wb <- openxlsx::createWorkbook()

estilo_titulo <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 14, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = cor_azul, halign = "left"
)
estilo_painel <- openxlsx::createStyle(
  fontName = "Arial", fontSize = 12, textDecoration = "bold",
  fontColour = "#FFFFFF", fgFill = cor_verde, halign = "left"
)
estilo_cabecalho <- openxlsx::createStyle(
  fontName = "Arial", textDecoration = "bold", fontColour = "#FFFFFF",
  fgFill = cor_azul, halign = "center", valign = "center", wrapText = TRUE,
  border = "Bottom", borderColour = "#FFFFFF"
)
estilo_inteiro <- openxlsx::createStyle(numFmt = "#,##0")
estilo_moeda <- openxlsx::createStyle(numFmt = "R$ #,##0.00")
estilo_percentual <- openxlsx::createStyle(numFmt = "0.00\"%\"")

paineis <- c(
  "Taxa de referência (TR)" = "Painel A — Taxa de referência (TR)",
  "Taxa fixa" = "Painel B — Taxa fixa"
)

for (origem_atual in c("Direta", "Indireta")) {
  nome_aba <- paste("Juros", origem_atual, sep = " - ")
  openxlsx::addWorksheet(wb, nome_aba, gridLines = FALSE)
  openxlsx::writeData(wb, nome_aba, paste("Juros — Apoio", origem_atual), startRow = 1)
  openxlsx::mergeCells(wb, nome_aba, cols = 1:8, rows = 1)
  openxlsx::addStyle(wb, nome_aba, estilo_titulo, rows = 1, cols = 1:8)

  linha <- 3
  tipos_presentes <- names(paineis)[
    names(paineis) %in% unique(dados_juros$tipo_custo[dados_juros$origem == origem_atual])
  ]

  for (tipo_atual in tipos_presentes) {
    tabela <- dados_juros |>
      filter(origem == origem_atual, tipo_custo == tipo_atual) |>
      select(-origem, -tipo_custo)

    openxlsx::writeData(wb, nome_aba, paineis[[tipo_atual]], startRow = linha)
    openxlsx::mergeCells(wb, nome_aba, cols = 1:8, rows = linha)
    openxlsx::addStyle(wb, nome_aba, estilo_painel, rows = linha, cols = 1:8)

    cabecalho <- linha + 1
    openxlsx::writeData(wb, nome_aba, tabela, startRow = cabecalho, withFilter = FALSE)
    openxlsx::addStyle(wb, nome_aba, estilo_cabecalho, rows = cabecalho, cols = 1:8)

    linhas_dados <- (cabecalho + 1):(cabecalho + nrow(tabela))
    openxlsx::addStyle(wb, nome_aba, estilo_inteiro, rows = linhas_dados, cols = c(1, 3), gridExpand = TRUE)
    openxlsx::addStyle(wb, nome_aba, estilo_moeda, rows = linhas_dados, cols = 4:6, gridExpand = TRUE)
    openxlsx::addStyle(wb, nome_aba, estilo_percentual, rows = linhas_dados, cols = 7:8, gridExpand = TRUE)

    linha <- cabecalho + nrow(tabela) + 3
  }

  openxlsx::setColWidths(wb, nome_aba, cols = 1, widths = 10)
  openxlsx::setColWidths(wb, nome_aba, cols = 2, widths = 22)
  openxlsx::setColWidths(wb, nome_aba, cols = 3, widths = 14)
  openxlsx::setColWidths(wb, nome_aba, cols = 4:8, widths = 24)
  openxlsx::freezePane(wb, nome_aba, firstActiveRow = 3)
}

openxlsx::saveWorkbook(wb, arquivo_saida, overwrite = TRUE)
message("Arquivo salvo em: ", arquivo_saida)
