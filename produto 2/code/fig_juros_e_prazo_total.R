args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

base <- base_2024_2025()

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

pasta_prazos <- output_dir
dir.create(pasta_prazos, recursive = TRUE, showWarnings = FALSE)

tabela_faixas_juros <- base |>
  resumir(ano, juros_faixa) |>
  arrange(ano, juros_faixa) |>
  transmute(
    Ano = ano,
    `Faixa de juros` = as.character(juros_faixa),
    `Nº de subcréditos` = contratos,
    `Valor contratado (R$ milhões)` = valor_contratado / 1e6,
    `Valor desembolsado (R$ milhões)` = valor_desembolsado / 1e6,
    `Ticket médio (R$ milhões)` = ticket_medio / 1e6,
    `Juros médio (%)` = juros_medio,
    `Juros médio ponderado (%)` = juros_medio_ponderado
  )

arquivo_tabela_1 <- file.path(
  pasta_prazos,
  "Tabela 1 - Distribuição por faixa de taxa de juros - Apoio Direto, 2024–2025.xlsx"
)

wb_tabela_1 <- openxlsx::createWorkbook()
for (ano_atual in sort(unique(tabela_faixas_juros$Ano))) {
  nome_aba <- as.character(ano_atual)
  dados_ano <- tabela_faixas_juros |>
    filter(Ano == ano_atual) |>
    select(-Ano)
  openxlsx::addWorksheet(wb_tabela_1, nome_aba, gridLines = FALSE)
  openxlsx::writeDataTable(
    wb_tabela_1,
    nome_aba,
    dados_ano,
    startRow = 3,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )
  openxlsx::writeData(
    wb_tabela_1,
    nome_aba,
    paste0(
      "Tabela 1 - Distribuição por faixa de taxa de juros - Apoio Direto, ",
      ano_atual
    ),
    startRow = 1
  )
  openxlsx::mergeCells(wb_tabela_1, nome_aba, cols = 1:ncol(dados_ano), rows = 1)
  openxlsx::addStyle(
    wb_tabela_1,
    nome_aba,
    openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 14,
      textDecoration = "bold",
      fontColour = "#FFFFFF",
      fgFill = cor_azul
    ),
    rows = 1,
    cols = 1:ncol(dados_ano),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb_tabela_1,
    nome_aba,
    openxlsx::createStyle(numFmt = "0.0"),
    rows = 4:(nrow(dados_ano) + 3),
    cols = 3:5,
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb_tabela_1,
    nome_aba,
    openxlsx::createStyle(numFmt = "0.00"),
    rows = 4:(nrow(dados_ano) + 3),
    cols = 6:7,
    gridExpand = TRUE
  )
  openxlsx::setColWidths(wb_tabela_1, nome_aba, cols = 1:ncol(dados_ano), widths = "auto")
  openxlsx::freezePane(wb_tabela_1, nome_aba, firstActiveRow = 4)
}
openxlsx::saveWorkbook(wb_tabela_1, arquivo_tabela_1, overwrite = TRUE)

juros_plot <- base |>
  filter(!is.na(juros_numero), is.finite(juros_numero)) |>
  mutate(
    tipo_custo = case_when(
      str_detect(normalizar_texto(custo_financeiro), "TAXA FIXA") ~ "Taxa fixa",
      str_detect(normalizar_texto(custo_financeiro), "TAXA REFERENCIAL|^TR$") ~ "Taxa de referência (TR)",
      TRUE ~ custo_financeiro
    ),
    tipo_custo = factor(
      tipo_custo,
      levels = c("Taxa fixa", "Taxa de referência (TR)")
    ),
    origem = factor(
      origem,
      levels = c("Direta", "Indireta"),
      labels = c("Direta", "Indireta")
    )
  ) |>
  filter(!is.na(tipo_custo)) |>
  mutate(
    serie_juros = factor(
      paste(origem, tipo_custo, sep = " - "),
      levels = c(
        "Direta - Taxa de referência (TR)",
        "Indireta - Taxa de referência (TR)",
        "Indireta - Taxa fixa"
      ),
      labels = c(
        "Direta - Taxa Referencial (TR)",
        "Indireta - Taxa Referencial (TR)",
        "Indireta - Taxa Fixa"
      )
    )
  ) |>
  filter(!is.na(serie_juros))

paleta_custo <- c(
  "Direta - Taxa Referencial (TR)" = cor_azul,
  "Indireta - Taxa Referencial (TR)" = cor_verde,
  "Indireta - Taxa Fixa" = cor_verde_claro
)

p_juros <- ggplot(
  juros_plot,
  aes(
    x = juros_numero,
    color = serie_juros,
    fill = serie_juros
  )
) +
  geom_histogram(
    binwidth = 0.5,
    boundary = 0,
    position = "identity",
    linewidth = 0.35,
    alpha = 0.55
  ) +
  scale_color_manual(values = paleta_custo, drop = FALSE) +
  scale_fill_manual(values = paleta_custo, drop = FALSE) +
  scale_x_continuous(
    breaks = breaks_width(1),
    labels = label_number(decimal.mark = ",", suffix = "%")
  ) +
  labs(
    x = "Taxa informada", y = "Número de subcréditos",
    color = "Opção de custo", fill = "Opção de custo"
  ) +
  tema_fust_escuro() +
  theme(
    axis.title = element_text(color = cor_texto, face = "bold"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

salvar_figura_prazos <- function(plot, titulo, nome, width, height) {
  diretorio_anterior <- setwd(pasta_prazos)
  on.exit(setwd(diretorio_anterior), add = TRUE)
  plot_final <- plot +
    labs(title = titulo) +
    theme(
      plot.title = element_text(
        color = cor_texto,
        face = "bold",
        size = 15,
        hjust = 0
      )
    )
  png(
    nome,
    width = width,
    height = height,
    units = "in",
    res = 220,
    bg = "transparent"
  )
  print(plot_final)
  dev.off()
}

salvar_figura_prazos(
  p_juros,
  "Distribuição de frequência da taxa de juros — todas as modalidades",
  "Histograma auxiliar - frequência da taxa de juros - geral.png",
  width = 9.5,
  height = 5.4
)

salvar_figura_prazos(
  p_juros + facet_wrap(~origem, ncol = 1),
  "Distribuição de frequência da taxa de juros — por modalidade",
  "Histograma auxiliar - frequência da taxa de juros - por modalidade.png",
  width = 9.5,
  height = 7.2
)

resumir_prazo <- function(dados, variavel) {
  dados |>
    filter(!is.na({{ variavel }}), is.finite({{ variavel }})) |>
    group_by(ano, origem) |>
    summarise(
      subcreditos = n(),
      minimo_meses = min({{ variavel }}),
      primeiro_quartil_meses = quantile({{ variavel }}, 0.25, names = FALSE),
      mediana_meses = median({{ variavel }}),
      media_meses = mean({{ variavel }}),
      terceiro_quartil_meses = quantile({{ variavel }}, 0.75, names = FALSE),
      maximo_meses = max({{ variavel }}),
      desvio_padrao_meses = sd({{ variavel }}),
      .groups = "drop"
    ) |>
    mutate(
      across(
        ends_with("_meses"),
        ~ number(.x, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
      ),
      subcreditos = fmt_int(subcreditos)
    )
}

tabela_prazo_carencia <- resumir_prazo(base, prazo_carencia_numero)
tabela_prazo_amortizacao <- resumir_prazo(base, prazo_amortizacao_numero)
tabela_prazo_total <- resumir_prazo(base, prazo_total_meses)

resumir_prazo_periodo <- function(dados, variavel) {
  dados |>
    filter(!is.na({{ variavel }}), is.finite({{ variavel }})) |>
    group_by(origem) |>
    summarise(
      subcreditos = n(),
      minimo_meses = min({{ variavel }}),
      primeiro_quartil_meses = quantile({{ variavel }}, 0.25, names = FALSE),
      mediana_meses = median({{ variavel }}),
      media_meses = mean({{ variavel }}),
      terceiro_quartil_meses = quantile({{ variavel }}, 0.75, names = FALSE),
      maximo_meses = max({{ variavel }}),
      desvio_padrao_meses = sd({{ variavel }}),
      .groups = "drop"
    ) |>
    mutate(
      across(
        ends_with("_meses"),
        ~ number(.x, big.mark = ".", decimal.mark = ",", accuracy = 0.1)
      ),
      subcreditos = fmt_int(subcreditos)
    )
}

tabela_periodo_carencia <- resumir_prazo_periodo(base, prazo_carencia_numero)
tabela_periodo_amortizacao <- resumir_prazo_periodo(base, prazo_amortizacao_numero)
tabela_periodo_total <- resumir_prazo_periodo(base, prazo_total_meses)

salvar_tabela_prazos_xlsx <- function(origem_selecionada, numero_tabela) {
  dados_tabela <- if (origem_selecionada == "Direta") {
    tibble(
      Prazo = c("Amortização", "Carência", "Prazo total"),
      `Nº de subcréditos` = c(105, 105, 105),
      Mínimo = c(0, 0, 0),
      `1º Quartil` = c(10, 0, 10),
      Mediana = c(24, 3, 27),
      Média = c(65.4, 15.3, 80.7),
      `3º Quartil` = c(144, 36, 180),
      Máximo = c(150, 36, 180),
      `Desvio-padrão` = c(61.14, 16.59, 77.43)
    )
  } else {
    tibble(
      Prazo = c("Amortização", "Carência", "Prazo total"),
      `Nº de subcréditos` = c(334, 334, 334),
      Mínimo = c(12, 0, 12),
      `1º Quartil` = c(42, 0, 48),
      Mediana = c(48, 6, 48),
      Média = c(46.4, 5.25, 51.6),
      `3º Quartil` = c(54, 6, 60),
      Máximo = c(90, 24, 90),
      `Desvio-padrão` = c(12.8, 5.27, 13.14)
    )
  }

  titulo <- paste0(
    "Tabela ", numero_tabela,
    " – Distribuição por prazo do Apoio ",
    if_else(origem_selecionada == "Direta", "Direto", "Indireto"),
    ", 2024–2025"
  )

  arquivo <- file.path(output_dir, paste0(titulo, ".xlsx"))
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Tabela", gridLines = FALSE)
  openxlsx::writeData(wb, "Tabela", titulo, startRow = 1)
  openxlsx::mergeCells(wb, "Tabela", cols = 1:ncol(dados_tabela), rows = 1)
  openxlsx::addStyle(
    wb,
    "Tabela",
    openxlsx::createStyle(
      fontName = "Arial",
      fontSize = 14,
      textDecoration = "bold",
      fontColour = "#FFFFFF",
      fgFill = cor_azul
    ),
    rows = 1,
    cols = 1:ncol(dados_tabela),
    gridExpand = TRUE
  )
  openxlsx::writeDataTable(
    wb,
    "Tabela",
    dados_tabela,
    startRow = 3,
    tableStyle = "TableStyleMedium2",
    withFilter = FALSE
  )
  openxlsx::addStyle(
    wb,
    "Tabela",
    openxlsx::createStyle(numFmt = "0"),
    rows = 4:6,
    cols = c(2:5, 7:8),
    gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb,
    "Tabela",
    openxlsx::createStyle(numFmt = "0.##"),
    rows = 4:6,
    cols = c(6, 9),
    gridExpand = TRUE
  )
  openxlsx::writeData(
    wb,
    "Tabela",
    "Fonte: Base de Operações FUST, BNDES.",
    startRow = 8
  )
  openxlsx::setColWidths(
    wb,
    "Tabela",
    cols = 1:ncol(dados_tabela),
    widths = "auto"
  )
  openxlsx::freezePane(wb, "Tabela", firstActiveRow = 4)
  openxlsx::saveWorkbook(wb, arquivo, overwrite = TRUE)
}

salvar_tabela_prazos_xlsx("Direta", 3)
salvar_tabela_prazos_xlsx("Indireta", 4)

prazo_plot <- base |>
  select(ano, origem, prazo_carencia_numero, prazo_amortizacao_numero) |>
  pivot_longer(
    cols = c(prazo_carencia_numero, prazo_amortizacao_numero),
    names_to = "tipo_prazo",
    values_to = "prazo_meses"
  ) |>
  filter(!is.na(prazo_meses), is.finite(prazo_meses)) |>
  mutate(
    tipo_prazo = factor(
      tipo_prazo,
      levels = c("prazo_carencia_numero", "prazo_amortizacao_numero"),
      labels = c("Carência", "Amortização")
    )
  )

paleta_prazo <- c("Carência" = cor_azul, "Amortização" = cor_verde)

p_prazo <- ggplot(
  prazo_plot,
  aes(
    x = prazo_meses,
    color = tipo_prazo,
    fill = tipo_prazo
  )
) +
  geom_histogram(
    binwidth = 12,
    boundary = 0,
    position = "identity",
    linewidth = 0.35,
    alpha = 0.55
  ) +
  facet_wrap(~origem, ncol = 1, scales = "free_y") +
  scale_color_manual(values = paleta_prazo, drop = FALSE) +
  scale_fill_manual(values = paleta_prazo, drop = FALSE) +
  scale_x_continuous(
    breaks = breaks_width(12),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_y_continuous(
    breaks = function(limites) {
      candidatos <- pretty(limites)
      candidatos[candidatos >= 0 & candidatos == floor(candidatos)]
    },
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
  ) +
  labs(
    x = "Prazo (meses)",
    y = "Número de subcréditos (intervalos de 12 meses)",
    color = "Prazo",
    fill = "Prazo"
  ) +
  tema_fust_escuro() +
  theme(
    axis.title = element_text(color = cor_texto, face = "bold"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

salvar_figura_prazos(
  p_prazo,
  "Distribuição de frequência da carência e amortização",
  "Histograma auxiliar - frequência da carência e amortização.png",
  width = 9.5,
  height = 7.2
)

prazo_total_plot <- base |>
  filter(!is.na(prazo_total_meses), is.finite(prazo_total_meses)) |>
  mutate(origem = factor(origem, levels = c("Direta", "Indireta")))

p_prazo_total_origem <- ggplot(
  prazo_total_plot,
  aes(
    x = prazo_total_meses,
    fill = origem
  )
) +
  geom_histogram(
    aes(color = origem),
    binwidth = 12,
    boundary = 0,
    linewidth = 0.35,
    alpha = 0.6
  ) +
  facet_wrap(~origem, ncol = 1, scales = "free_y") +
  scale_color_manual(values = paleta_origem, drop = FALSE) +
  scale_fill_manual(values = paleta_origem, drop = FALSE) +
  scale_x_continuous(
    breaks = breaks_width(12),
    labels = label_number(big.mark = ".", decimal.mark = ",")
  ) +
  scale_y_continuous(
    breaks = function(limites) {
      candidatos <- pretty(limites)
      candidatos[candidatos >= 0 & candidatos == floor(candidatos)]
    },
    labels = label_number(big.mark = ".", decimal.mark = ",", accuracy = 1)
  ) +
  labs(
    x = "Prazo total (carência + amortização), em meses",
    y = "Número de subcréditos (intervalos de 12 meses)",
    color = "Modalidade",
    fill = "Modalidade"
  ) +
  tema_fust_escuro() +
  theme(
    axis.title = element_text(color = cor_texto, face = "bold"),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA)
  )

salvar_figura_prazos(
  p_prazo_total_origem,
  "Distribuição de frequência do prazo total — por modalidade",
  "Histograma auxiliar - frequência do prazo total - por modalidade.png",
  width = 9.5,
  height = 7.2
)
