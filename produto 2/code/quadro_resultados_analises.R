args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

pacotes_qr <- c("gridExtra", "janitor", "openxlsx", "writexl")
faltando_qr <- setdiff(pacotes_qr, rownames(installed.packages()))
if (length(faltando_qr) > 0) {
  install.packages(faltando_qr)
}

suppressPackageStartupMessages({
  library(gridExtra)
  library(janitor)
  library(openxlsx)
  library(writexl)
})

qr_out_dir <- file.path(
  output_dir,
  "2.1 Apoio Direto - consolidação e análise dos indicadores de Eficácia e Efetividade"
)
dir.create(qr_out_dir, recursive = TRUE, showWarnings = FALSE)
qr_indicadores_dir <- file.path(qr_out_dir, "indicadores")
dir.create(qr_indicadores_dir, recursive = TRUE, showWarnings = FALSE)
qr_indicadores_uf_dir <- file.path(qr_out_dir, "indicadores por UF")
dir.create(qr_indicadores_uf_dir, recursive = TRUE, showWarnings = FALSE)

arquivo_qr_original <- file.path(
  dirname(root_dir),
  "Produto 2",
  "base_atualizada",
  "Base de QR FUST 2.xlsx"
)
arquivo_qr_copia <- file.path(data_dir, "Base de QR FUST 2.xlsx")
arquivo_qr <- if (file.exists(arquivo_qr_original)) {
  arquivo_qr_original
} else {
  arquivo_qr_copia
}

if (!file.exists(arquivo_qr)) {
  stop("Base de QR FUST 2.xlsx não encontrada.", call. = FALSE)
}

salvar_csv_qr <- function(df, nome) {
  readr::write_excel_csv2(df, file.path(qr_out_dir, nome), na = "")
}

salvar_figura_qr <- function(plot, nome, width = 9, height = 5.5) {
  ggsave(
    file.path(qr_out_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "transparent"
  )
}

salvar_figura_indicador <- function(plot, nome, width = 8.5, height = 4.8) {
  ggsave(
    file.path(qr_indicadores_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "transparent"
  )
}

salvar_figura_indicador_uf <- function(plot, nome, width = 9.5, height = 5.8) {
  ggsave(
    file.path(qr_indicadores_uf_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "transparent"
  )
}

primeiro_texto <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_squish(x) != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  first(x)
}

fmt_valor_indicador <- function(x) {
  case_when(
    abs(x) >= 1e9 ~ paste0(number(x / 1e9, decimal.mark = ",", accuracy = 0.1), " bi"),
    abs(x) >= 1e6 ~ paste0(number(x / 1e6, decimal.mark = ",", accuracy = 0.1), " mi"),
    abs(x) >= 1e3 ~ paste0(number(x / 1e3, decimal.mark = ",", accuracy = 0.1), " mil"),
    abs(x) > 0 & abs(x) < 1 ~ number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.01),
    TRUE ~ number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
  )
}

fmt_pct_ratio <- function(x) {
  paste0(number(100 * x, big.mark = ".", decimal.mark = ",", accuracy = 0.1), "%")
}

fmt_tabela_indicador <- function(x) {
  case_when(
    is.na(x) ~ "",
    abs(x) > 0 & abs(x) < 1 ~ number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.01),
    TRUE ~ number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
  )
}

classificar_cobertura <- function(x) {
  case_when(
    is.na(x) ~ "Sem previsto",
    x >= 1 ~ "100% ou mais",
    x >= 0.8 ~ "80% a 99%",
    x >= 0.5 ~ "50% a 79%",
    TRUE ~ "Abaixo de 50%"
  )
}

indicador_de_contagem <- function(x) {
  valor <- normalizar_texto(x)
  str_detect(valor, "\\b(NUMERO|N|NO|QTD|QUANTIDADE)\\b") &
    !str_detect(valor, "R MIL|ROL|RECEITA")
}

sintetizar_indicador <- function(x) {
  texto <- str_squish(as.character(x))
  texto <- str_replace_all(texto, "Moderniza[cç][aã]o da infraestrutura tecnol[oó]gica:\\s*", "")
  texto <- str_replace_all(texto, "Moderniza[cç][aã]o de infraestrutura tecnol[oó]gica:\\s*", "")
  texto <- str_replace_all(texto, "Moderniza[cç][aã]o da infraestrutura:\\s*", "")
  texto <- str_replace_all(texto, "\\s*\\((n[uú]mero|n[ºo]) de ([^)]+)\\)", "")
  texto <- str_replace_all(texto, "\\s*\\(R\\$\\s*mil/?ano\\)", " (R$ mil/ano)")
  texto <- str_replace_all(texto, "Localidades com atendimento inadequado de Backhaul \\(segundo crit[eé]rios da ANATEL\\) que tiveram sua rede de banda larga fixa expandida", "Localidades com backhaul expandido")
  texto <- str_replace_all(texto, "Munic[ií]pios com atendimento inadequado de Backhaul \\(segundo crit[eé]rios da ANATEL\\) que tiveram sua rede de banda larga fixa expandida", "Municípios com backhaul expandido")
  texto <- str_replace_all(texto, "Localidades com cobertura inadequada de SMP \\(segundo crit[eé]rios da ANATEL\\) que tiveram sua cobertura expandida", "Localidades com cobertura SMP expandida")
  texto <- str_replace_all(texto, "Munic[ií]pios que tiveram teledensidade de banda larga fixa aumentada", "Municípios com teledensidade BL fixa aumentada")
  texto <- str_replace_all(texto, "Escolas conectadas [àa] rede de internet de banda larga", "Escolas conectadas à banda larga")
  texto <- str_replace_all(texto, "Escolas conectadas [àa] rede para uso pedag[oó]gico", "Escolas conectadas para uso pedagógico")
  texto <- str_replace_all(texto, "Escolas com rede interna atualizada", "Escolas com rede interna atualizada")
  texto <- str_replace_all(texto, "Velocidade da rede de internet de banda larga nas escolas", "Velocidade da banda larga nas escolas")
  texto <- str_replace_all(texto, "Cr[eé]dito de uso livre aplicado na empresa", "Crédito livre aplicado")
  texto <- str_replace_all(texto, "Esta[cç][oõ]es R[aá]dio Base \\(ERB\\) 5G licenciadas", "ERBs 5G licenciadas")
  texto <- str_replace_all(texto, "Home passed instalados", "Home passed")
  texto <- str_replace_all(texto, "Extens[aã]o de fibra [oó]tica instalada", "Fibra óptica instalada")
  texto <- str_replace_all(texto, "Torres de telecomunica[cç][oõ]es", "Torres telecom")
  texto <- str_replace_all(texto, "Acessos de banda larga fixa", "Acessos BL fixa")
  texto <- str_replace_all(texto, "Receita Operacional L[ií]quida - ROL", "ROL")
  texto <- str_replace_all(texto, "Emprego", "Empregos")
  str_squish(texto)
}

alerta_unidade_indicador <- function(indicador, previsto, atingido) {
  alerta <- indicador_de_contagem(indicador) &
    (
      (!is.na(previsto) & previsto > 0 & previsto < 1) |
        (!is.na(atingido) & atingido > 0 & atingido < 1)
    )

  if_else(
    alerta,
    paste(
      "Verificar unidade: indicador de contagem com valor fracionario",
      "menor que 1 na base."
    ),
    NA_character_
  )
}

nome_arquivo_indicador <- function(indicador, indice) {
  slug <- normalizar_texto(indicador) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "-") |>
    str_replace_all("^-|-$", "") |>
    str_trunc(80, side = "right", ellipsis = "")

  paste0(
    str_pad(indice, width = 2, pad = "0"),
    " - ",
    slug,
    ".png"
  )
}

alinhar_primeira_coluna <- function(grob_tabela) {
  idx <- which(
    grob_tabela$layout$l == 1 &
      str_detect(grob_tabela$layout$name, "fg")
  )

  for (i in idx) {
    grob_tabela$grobs[[i]]$x <- unit(0.02, "npc")
    grob_tabela$grobs[[i]]$hjust <- 0
  }

  grob_tabela
}

salvar_tabelao_resultados_xlsx <- function(tabelas, caminho) {
  wb <- openxlsx::createWorkbook()
  estilo_titulo <- openxlsx::createStyle(
    fontSize = 14,
    textDecoration = "bold",
    fontColour = cor_azul
  )
  estilo_subtitulo <- openxlsx::createStyle(
    fontSize = 10,
    fontColour = cor_texto,
    wrapText = TRUE,
    valign = "top"
  )
  estilo_cabecalho <- openxlsx::createStyle(
    fgFill = "#7F7F7F",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "TopBottomLeftRight",
    borderColour = "#000000",
    wrapText = TRUE
  )
  estilo_texto <- openxlsx::createStyle(
    valign = "top",
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderColour = "#000000"
  )
  estilo_numero <- openxlsx::createStyle(
    numFmt = "#,##0.00",
    halign = "right",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000"
  )
  estilo_percentual <- openxlsx::createStyle(
    numFmt = "0.0%",
    halign = "right",
    valign = "top",
    border = "TopBottomLeftRight",
    borderColour = "#000000"
  )
  estilo_eficacia <- openxlsx::createStyle(fgFill = "#EAF2F1")
  estilo_efetividade <- openxlsx::createStyle(fgFill = "#F4F7F6")

  for (nome_aba in names(tabelas)) {
    item <- tabelas[[nome_aba]]
    dados <- item$dados
    openxlsx::addWorksheet(wb, nome_aba)
    openxlsx::writeData(wb, nome_aba, item$titulo, startRow = 1, startCol = 1)
    openxlsx::mergeCells(wb, nome_aba, cols = 1:ncol(dados), rows = 1)
    openxlsx::addStyle(wb, nome_aba, estilo_titulo, rows = 1, cols = 1, stack = TRUE)

    if (!is.null(item$subtitulo) && item$subtitulo != "") {
      openxlsx::writeData(wb, nome_aba, item$subtitulo, startRow = 2, startCol = 1)
      openxlsx::mergeCells(wb, nome_aba, cols = 1:ncol(dados), rows = 2)
      openxlsx::addStyle(wb, nome_aba, estilo_subtitulo, rows = 2, cols = 1, stack = TRUE)
    }

    openxlsx::writeData(wb, nome_aba, dados, startRow = 4, startCol = 1, withFilter = TRUE)
    openxlsx::addStyle(
      wb,
      nome_aba,
      estilo_cabecalho,
      rows = 4,
      cols = seq_len(ncol(dados)),
      gridExpand = TRUE,
      stack = TRUE
    )

    linhas_dados <- 5:(nrow(dados) + 4)
    if (length(linhas_dados) > 0) {
      cols_texto <- seq_len(min(4, ncol(dados)))
      cols_anos <- setdiff(seq_len(ncol(dados)), cols_texto)
      openxlsx::addStyle(
        wb,
        nome_aba,
        estilo_texto,
        rows = linhas_dados,
        cols = cols_texto,
        gridExpand = TRUE,
        stack = TRUE
      )
      if (length(cols_anos) > 0) {
        openxlsx::addStyle(
          wb,
          nome_aba,
          if (isTRUE(item$percentual)) estilo_percentual else estilo_numero,
          rows = linhas_dados,
          cols = cols_anos,
          gridExpand = TRUE,
          stack = TRUE
        )
      }

      linhas_eficacia <- which(dados[[1]] == "Produtos e serviços (eficácia)") + 4
      linhas_efetividade <- which(dados[[1]] == "Objetivos específicos (efetividade)") + 4
      if (length(linhas_eficacia) > 0) {
        openxlsx::addStyle(
          wb,
          nome_aba,
          estilo_eficacia,
          rows = linhas_eficacia,
          cols = seq_len(ncol(dados)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
      if (length(linhas_efetividade) > 0) {
        openxlsx::addStyle(
          wb,
          nome_aba,
          estilo_efetividade,
          rows = linhas_efetividade,
          cols = seq_len(ncol(dados)),
          gridExpand = TRUE,
          stack = TRUE
        )
      }
    }

    openxlsx::setColWidths(wb, nome_aba, cols = 1, widths = 24)
    openxlsx::setColWidths(wb, nome_aba, cols = 2, widths = 22)
    openxlsx::setColWidths(wb, nome_aba, cols = 3, widths = 34)
    openxlsx::setColWidths(wb, nome_aba, cols = 4, widths = 70)
    if (ncol(dados) > 4) {
      openxlsx::setColWidths(wb, nome_aba, cols = 5:ncol(dados), widths = 14)
    }
    openxlsx::freezePane(wb, nome_aba, firstActiveRow = 5, firstActiveCol = 5)
  }

  openxlsx::saveWorkbook(wb, caminho, overwrite = TRUE)
}

qr <- read_excel(arquivo_qr) |>
  clean_names() |>
  transmute(
    numero_operacao = str_remove(as.character(operacao_numero), "\\.0$"),
    eixo = operacao_qr_classe_n,
    data_linha_base = as.Date(operacao_qr_indicador_base_data_n),
    valor_linha_base = as.numeric(operacao_qr_indicador_base_valor_n),
    indicador = operacao_qr_indicador_nome_n,
    tipo_indicador = operacao_qr_indicador_tipo_n,
    objetivo = operacao_qr_objetivo_n,
    data_prevista = as.Date(operacao_qr_indicador_previsto_data_n),
    valor_previsto = as.numeric(operacao_qr_indicador_previsto_valor_n),
    tipo_preenchimento = as.character(operacao_qr_tipo_preenchimento_n),
    instrumento_financeiro_qr = subcredito_instrumento_financeiro,
    fonte_recurso_qr = liberacao_fonte_recurso_sigla,
    valor_atingido_indicador = as.numeric(valor_indicador),
    uf_qr = str_to_upper(uf_operacao),
    data_aprovacao = as.Date(data_aprovacao),
    ano_aprovacao = as.integer(ano_aprovacao)
  ) |>
  mutate(
    valor_meta_calculado = case_when(
      tipo_preenchimento == "3" &
        !is.na(valor_linha_base) &
        !is.na(valor_previsto) ~ valor_previsto - valor_linha_base,
      TRUE ~ valor_previsto
    ),
    meta_consistente = near(
      valor_atingido_indicador,
      valor_meta_calculado,
      tol = 1e-8
    ),
    cobertura_indicador = if_else(
      valor_previsto > 0,
      valor_atingido_indicador / valor_previsto,
      NA_real_
    ),
    realizado_indicador_disponivel = !is.na(valor_atingido_indicador),
    ano_indicador = coalesce(
      as.integer(format(data_prevista, "%Y")),
      ano_aprovacao
    ),
    uf_analise = if_else(uf_qr == "IE", "Interestadual (IE)", uf_qr)
  )

base_fust_operacao <- ler_base() |>
  filter(!is.na(numero_de_operacao)) |>
  mutate(
    numero_operacao = str_remove(as.character(numero_de_operacao), "\\.0$")
  ) |>
  group_by(numero_operacao) |>
  summarise(
    cliente = primeiro_texto(cliente),
    cnpj = primeiro_texto(cnpj),
    uf_fust = primeiro_texto(uf_original),
    municipio_fust = primeiro_texto(municipio),
    data_contratacao = min(data, na.rm = TRUE),
    subcreditos = n_distinct(subcredito, na.rm = TRUE),
    valor_contratado = sum(valor_contratado, na.rm = TRUE),
    valor_desembolsado = sum(valor_desembolsado, na.rm = TRUE),
    instrumento_financeiro_fust = primeiro_texto(instrumento_financeiro),
    produto_fust = primeiro_texto(produto),
    .groups = "drop"
  ) |>
  mutate(
    percentual_desembolsado = if_else(
      valor_contratado > 0,
      valor_desembolsado / valor_contratado,
      NA_real_
    )
  )

base_integrada <- qr |>
  left_join(base_fust_operacao, by = "numero_operacao") |>
  mutate(
    join_encontrado = !is.na(cliente),
    uf_analise = coalesce(uf_analise, uf_fust)
  )

cobertura_join <- tibble(
  fonte_qr = normalizePath(arquivo_qr),
  indicadores_qr = nrow(qr),
  operacoes_qr = n_distinct(qr$numero_operacao),
  operacoes_encontradas_fust = n_distinct(
    base_integrada$numero_operacao[base_integrada$join_encontrado]
  ),
  operacoes_nao_encontradas_fust = n_distinct(
    base_integrada$numero_operacao[!base_integrada$join_encontrado]
  ),
  percentual_operacoes_encontradas = operacoes_encontradas_fust / operacoes_qr,
  metas_consistentes = sum(base_integrada$meta_consistente, na.rm = TRUE),
  metas_inconsistentes = sum(!base_integrada$meta_consistente, na.rm = TRUE),
  realizados_indicadores_disponiveis = sum(
    base_integrada$realizado_indicador_disponivel
  )
)

resumo_geral <- base_integrada |>
  group_by(tipo_indicador, eixo) |>
  summarise(
    registros_indicadores = n(),
    indicadores_distintos = n_distinct(indicador),
    operacoes = n_distinct(numero_operacao),
    ufs = n_distinct(uf_analise),
    indicadores_com_linha_base = sum(!is.na(valor_linha_base)),
    indicadores_com_previsto = sum(!is.na(valor_previsto)),
    indicadores_com_realizado = sum(realizado_indicador_disponivel),
    .groups = "drop"
  ) |>
  arrange(tipo_indicador, eixo)

resumir_indicadores <- function(tipo) {
  base_integrada |>
    filter(tipo_indicador == tipo) |>
    group_by(eixo, indicador) |>
    summarise(
      registros = n(),
      operacoes = n_distinct(numero_operacao),
      ufs = n_distinct(uf_analise),
      valores_linha_base_disponiveis = sum(!is.na(valor_linha_base)),
      soma_linha_base = sum(valor_linha_base, na.rm = TRUE),
      valores_previstos_disponiveis = sum(!is.na(valor_previsto)),
      soma_prevista = sum(valor_previsto, na.rm = TRUE),
      realizados_disponiveis = sum(realizado_indicador_disponivel),
      soma_atingida = sum(valor_atingido_indicador, na.rm = TRUE),
      cobertura = if_else(soma_prevista > 0, soma_atingida / soma_prevista, NA_real_),
      .groups = "drop"
    ) |>
    arrange(eixo, desc(registros), indicador)
}

resumir_indicadores_uf <- function(tipo) {
  base_integrada |>
    filter(tipo_indicador == tipo) |>
    group_by(uf_analise, eixo, indicador) |>
    summarise(
      registros = n(),
      operacoes = n_distinct(numero_operacao),
      soma_linha_base = sum(valor_linha_base, na.rm = TRUE),
      soma_prevista = sum(valor_previsto, na.rm = TRUE),
      soma_atingida = sum(valor_atingido_indicador, na.rm = TRUE),
      cobertura = if_else(soma_prevista > 0, soma_atingida / soma_prevista, NA_real_),
      realizados_disponiveis = sum(realizado_indicador_disponivel),
      .groups = "drop"
    ) |>
    arrange(uf_analise, eixo, desc(registros), indicador)
}

eficacia_eixo <- resumo_geral |>
  filter(tipo_indicador == "Eficácia")
eficacia_indicador <- resumir_indicadores("Eficácia")
eficacia_uf <- resumir_indicadores_uf("Eficácia")

efetividade_eixo <- resumo_geral |>
  filter(tipo_indicador == "Efetividade")
efetividade_indicador <- resumir_indicadores("Efetividade")
efetividade_uf <- resumir_indicadores_uf("Efetividade")

operacao_eixo <- base_integrada |>
  distinct(
    numero_operacao,
    eixo,
    uf_analise,
    cliente,
    valor_contratado,
    valor_desembolsado
  )

financeiro_eixo <- operacao_eixo |>
  group_by(eixo) |>
  summarise(
    operacoes = n_distinct(numero_operacao),
    valor_previsto_financeiro = sum(valor_contratado, na.rm = TRUE),
    valor_realizado_financeiro = sum(valor_desembolsado, na.rm = TRUE),
    diferenca_financeira = valor_previsto_financeiro - valor_realizado_financeiro,
    percentual_realizado = if_else(
      valor_previsto_financeiro > 0,
      valor_realizado_financeiro / valor_previsto_financeiro,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  arrange(eixo)

financeiro_uf_eixo <- operacao_eixo |>
  group_by(uf_analise, eixo) |>
  summarise(
    operacoes = n_distinct(numero_operacao),
    valor_previsto_financeiro = sum(valor_contratado, na.rm = TRUE),
    valor_realizado_financeiro = sum(valor_desembolsado, na.rm = TRUE),
    diferenca_financeira = valor_previsto_financeiro - valor_realizado_financeiro,
    percentual_realizado = if_else(
      valor_previsto_financeiro > 0,
      valor_realizado_financeiro / valor_previsto_financeiro,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  arrange(uf_analise, eixo)

financeiro_uf <- base_integrada |>
  distinct(
    numero_operacao,
    uf_analise,
    cliente,
    valor_contratado,
    valor_desembolsado
  ) |>
  group_by(uf_analise) |>
  summarise(
    operacoes = n_distinct(numero_operacao),
    valor_previsto_financeiro = sum(valor_contratado, na.rm = TRUE),
    valor_realizado_financeiro = sum(valor_desembolsado, na.rm = TRUE),
    diferenca_financeira = valor_previsto_financeiro - valor_realizado_financeiro,
    percentual_realizado = if_else(
      valor_previsto_financeiro > 0,
      valor_realizado_financeiro / valor_previsto_financeiro,
      NA_real_
    ),
    .groups = "drop"
  ) |>
  arrange(desc(valor_previsto_financeiro))

disponibilidade_realizado <- base_integrada |>
  group_by(tipo_indicador, eixo) |>
  summarise(
    indicadores = n(),
    previsto_disponivel = sum(!is.na(valor_previsto)),
    atingido_disponivel = sum(!is.na(valor_atingido_indicador)),
    realizado_disponivel = sum(realizado_indicador_disponivel),
    observacao = paste(
      "A base informa linha de base, nível previsto e valor.indicador.",
      "Nesta análise, valor.indicador foi tratado como valor atingido."
    ),
    .groups = "drop"
  )

indicador_ano <- base_integrada |>
  group_by(ano_indicador, indicador) |>
  summarise(
    tipo_indicador = if_else(
      n_distinct(tipo_indicador) == 1,
      first(tipo_indicador),
      "Misto"
    ),
    eixo = paste(sort(unique(eixo)), collapse = " | "),
    registros = n(),
    operacoes = n_distinct(numero_operacao),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    diferenca_atingido_previsto = valor_atingido - valor_previsto,
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    classe_cobertura = classificar_cobertura(cobertura),
    alerta_unidade = alerta_unidade_indicador(
      first(indicador),
      valor_previsto,
      valor_atingido
    ),
    .groups = "drop"
  ) |>
  arrange(indicador, ano_indicador)

indicador_total <- indicador_ano |>
  group_by(indicador) |>
  summarise(
    tipo_indicador = if_else(
      n_distinct(tipo_indicador) == 1,
      first(tipo_indicador),
      "Misto"
    ),
    eixo = paste(sort(unique(eixo)), collapse = " | "),
    anos = n_distinct(ano_indicador),
    registros = sum(registros, na.rm = TRUE),
    operacoes = sum(operacoes, na.rm = TRUE),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido, na.rm = TRUE),
    diferenca_atingido_previsto = valor_atingido - valor_previsto,
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    classe_cobertura = classificar_cobertura(cobertura),
    alerta_unidade = alerta_unidade_indicador(
      first(indicador),
      valor_previsto,
      valor_atingido
    ),
    .groups = "drop"
  ) |>
  arrange(cobertura, tipo_indicador, indicador)

indicador_uf <- base_integrada |>
  group_by(indicador, uf_analise) |>
  summarise(
    tipo_indicador = if_else(
      n_distinct(tipo_indicador) == 1,
      first(tipo_indicador),
      "Misto"
    ),
    eixo = paste(sort(unique(eixo)), collapse = " | "),
    registros = n(),
    operacoes = n_distinct(numero_operacao),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    diferenca_atingido_previsto = valor_atingido - valor_previsto,
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    classe_cobertura = classificar_cobertura(cobertura),
    alerta_unidade = alerta_unidade_indicador(
      first(indicador),
      valor_previsto,
      valor_atingido
    ),
    .groups = "drop"
  ) |>
  arrange(indicador, desc(valor_previsto), uf_analise)

anos_qr_disponiveis <- sort(unique(base_integrada$ano_aprovacao[!is.na(base_integrada$ano_aprovacao)]))

tabelao_resultados_long <- base_integrada |>
  filter(!is.na(ano_aprovacao)) |>
  mutate(
    tipo_indicador_norm = normalizar_texto(tipo_indicador),
    nivel_resultado = case_when(
      tipo_indicador_norm == "EFICACIA" ~ "Produtos e serviços (eficácia)",
      tipo_indicador_norm == "EFETIVIDADE" ~ "Objetivos específicos (efetividade)",
      TRUE ~ tipo_indicador
    ),
    ordem_nivel = case_when(
      tipo_indicador_norm == "EFICACIA" ~ 1L,
      tipo_indicador_norm == "EFETIVIDADE" ~ 2L,
      TRUE ~ 3L
    ),
    resultado = eixo
  ) |>
  group_by(
    ordem_nivel,
    nivel_resultado,
    resultado,
    objetivo,
    indicador,
    ano_aprovacao
  ) |>
  summarise(
    registros = n(),
    operacoes = n_distinct(numero_operacao),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    .groups = "drop"
  ) |>
  complete(
    nesting(ordem_nivel, nivel_resultado, resultado, objetivo, indicador),
    ano_aprovacao = anos_qr_disponiveis,
    fill = list(
      registros = 0,
      operacoes = 0,
      valor_previsto = 0,
      valor_atingido = 0
    )
  ) |>
  mutate(
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_)
  ) |>
  arrange(ordem_nivel, resultado, objetivo, indicador, ano_aprovacao)

montar_tabelao_resultados <- function(metrica) {
  tabelao_resultados_long |>
    select(
      ordem_nivel,
      `Nível resultado` = nivel_resultado,
      Resultado = resultado,
      Objetivo = objetivo,
      Indicador = indicador,
      ano_aprovacao,
      valor = all_of(metrica)
    ) |>
    pivot_wider(
      names_from = ano_aprovacao,
      values_from = valor,
      values_fill = if (metrica == "cobertura") NA_real_ else 0
    ) |>
    arrange(ordem_nivel, Resultado, Objetivo, Indicador) |>
    select(-ordem_nivel)
}

tabelao_resultados_previsto <- montar_tabelao_resultados("valor_previsto")
tabelao_resultados_atingido <- montar_tabelao_resultados("valor_atingido")
tabelao_resultados_cobertura <- montar_tabelao_resultados("cobertura")
titulo_tabelao_resultados_texto <- paste0(
  "Indicadores de resultados do Quadro de Resultados por nível, eixo e ano de aprovação (",
  min(anos_qr_disponiveis),
  " a ",
  max(anos_qr_disponiveis),
  ")"
)

anos_telecom_tabela <- sort(unique(base_integrada$ano_aprovacao[
  base_integrada$eixo == "TICs - Telecom e data center" &
    !is.na(base_integrada$ano_aprovacao)
]))
periodo_telecom <- paste(min(anos_telecom_tabela), max(anos_telecom_tabela), sep = " a ")
titulo_tabela_telecom_texto <- paste0(
  "TABELA 23. indicadores de resultados relativos a telecomunicacoes nas operacoes aprovadas de ",
  periodo_telecom
)

telecom_base_aprovacao <- base_integrada |>
  filter(
    eixo == "TICs - Telecom e data center"
  )

telecom_aprovacao <- telecom_base_aprovacao |>
  group_by(indicador, ano_aprovacao) |>
  summarise(
    registros = n(),
    operacoes = n_distinct(numero_operacao),
    ufs = n_distinct(uf_analise),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    alerta_unidade = alerta_unidade_indicador(
      first(indicador),
      valor_previsto,
      valor_atingido
    ),
    .groups = "drop"
  ) |>
  complete(
    indicador,
    ano_aprovacao = anos_telecom_tabela,
    fill = list(
      registros = 0,
      operacoes = 0,
      ufs = 0,
      valor_previsto = 0,
      valor_atingido = 0
    )
  )

tabela_telecom_resultados_por_ano <- telecom_aprovacao |>
  mutate(
    diferenca_atingido_previsto = valor_atingido - valor_previsto,
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    classe_cobertura = classificar_cobertura(cobertura)
  ) |>
  arrange(ano_aprovacao, indicador)

telecom_aprovacao_ano <- telecom_base_aprovacao |>
  group_by(ano_aprovacao) |>
  summarise(
    registros_indicadores = n(),
    indicadores_distintos = n_distinct(indicador),
    operacoes = n_distinct(numero_operacao),
    ufs = n_distinct(uf_analise),
    indicadores_com_previsto = sum(!is.na(valor_previsto)),
    indicadores_com_atingido = sum(realizado_indicador_disponivel),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    observacao = paste(
      "Valores previstos e atingidos somam indicadores com unidades diferentes;",
      "usar a tabela por indicador para interpretações de magnitude."
    ),
    .groups = "drop"
  ) |>
  complete(
    ano_aprovacao = anos_telecom_tabela,
    fill = list(
      registros_indicadores = 0,
      indicadores_distintos = 0,
      operacoes = 0,
      ufs = 0,
      indicadores_com_previsto = 0,
      indicadores_com_atingido = 0,
      valor_previsto = 0,
      valor_atingido = 0,
      observacao = paste(
        "Sem registros de indicadores de telecomunicações para operações aprovadas neste ano na Base de QR FUST 2."
      )
    )
  ) |>
  mutate(
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_)
  ) |>
  arrange(ano_aprovacao)

tabela_telecom_resultados <- telecom_aprovacao |>
  select(indicador, ano_aprovacao, valor_previsto, valor_atingido) |>
  pivot_wider(
    names_from = ano_aprovacao,
    values_from = c(valor_previsto, valor_atingido),
    values_fill = 0
  ) |>
  mutate(
    across(
      all_of(paste0("valor_previsto_", anos_telecom_tabela)),
      ~coalesce(.x, 0)
    ),
    across(
      all_of(paste0("valor_atingido_", anos_telecom_tabela)),
      ~coalesce(.x, 0)
    ),
    valor_previsto_total = rowSums(across(starts_with("valor_previsto_")), na.rm = TRUE),
    valor_atingido_total = rowSums(across(starts_with("valor_atingido_")), na.rm = TRUE)
  ) |>
  left_join(
    telecom_aprovacao |>
      group_by(indicador) |>
      summarise(
        alerta_unidade = primeiro_texto(alerta_unidade),
        .groups = "drop"
      ),
    by = "indicador"
  ) |>
  arrange(indicador)

titulo_tabela_educacao_texto <- "TABELA 30. indicadores de resultados relativos à educação nas operações aprovadas em 2022 e 2023"

tabela_educacao_resultados <- tibble(
  indicador = c(
    "Alunos a serem beneficiados (nº de pessoas)",
    "Unidades de ensino a serem beneficiadas (nº de estabelecimentos de ensino)",
    "Redes de ensino a serem beneficiadas (nº de redes de ensino)",
    "Formação de gestores escolares (nº de pessoas)",
    "Formação para atividades pedagógicas (nº de pessoas)",
    "Acesso adequado a escolas a esgotamento sanitário (nº de estabelecimentos de ensino)",
    "Conexão de escolas à rede de internet de banda larga (nº de estabelecimentos de ensino)"
  ),
  valor_previsto_2022 = c(823151, 4552, 330, 5480, 73798, 460, 7),
  valor_previsto_2023 = c(1321752, 2546, 10, 0, 0, 0, 125),
  ods_associados = c("4, 1", "4, 1", "4, 1", "4, 1", "4, 1", "4, 1, 6", "4, 1")
) |>
  mutate(
    valor_previsto_total = valor_previsto_2022 + valor_previsto_2023,
    observacao = paste(
      "Em relação aos dados da tabela, cabem as seguintes considerações:",
      "(i) os perfis das operações variam ano a ano, em função de objetivo,",
      "abrangência, território e valor;",
      "(ii) em 2023, as operações aprovadas tinham perfis distintos,",
      "com abrangência regional e setorial mais específicos, em comparação com 2022."
    )
  ) |>
  select(
    indicador,
    valor_previsto_2022,
    valor_previsto_2023,
    valor_previsto_total,
    ods_associados,
    observacao
  )

tabela_educacao_resultados_por_ano <- tabela_educacao_resultados |>
  select(
    indicador,
    valor_previsto_2022,
    valor_previsto_2023,
    ods_associados,
    observacao
  ) |>
  pivot_longer(
    c(valor_previsto_2022, valor_previsto_2023),
    names_to = "ano_aprovacao",
    names_pattern = "valor_previsto_(\\d+)",
    values_to = "valor_previsto"
  ) |>
  mutate(ano_aprovacao = as.integer(ano_aprovacao)) |>
  arrange(ano_aprovacao, indicador)

educacao_aprovacao_ano <- tabela_educacao_resultados_por_ano |>
  group_by(ano_aprovacao) |>
  summarise(
    indicadores = n(),
    indicadores_com_valor_previsto = sum(valor_previsto > 0, na.rm = TRUE),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    observacao = primeiro_texto(observacao),
    .groups = "drop"
  ) |>
  arrange(ano_aprovacao)

educacao_base_aprovacao <- base_integrada |>
  filter(eixo == "Educação")

anos_educacao_tabela <- sort(unique(educacao_base_aprovacao$ano_aprovacao[!is.na(educacao_base_aprovacao$ano_aprovacao)]))
periodo_educacao <- paste(min(anos_educacao_tabela), max(anos_educacao_tabela), sep = " a ")
titulo_tabela_educacao_texto <- paste0(
  "TABELA 30. indicadores de resultados relativos a educacao nas operacoes aprovadas de ",
  periodo_educacao
)

educacao_aprovacao <- educacao_base_aprovacao |>
  group_by(indicador, ano_aprovacao) |>
  summarise(
    registros = n(),
    operacoes = n_distinct(numero_operacao),
    ufs = n_distinct(uf_analise),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    alerta_unidade = alerta_unidade_indicador(
      first(indicador),
      valor_previsto,
      valor_atingido
    ),
    .groups = "drop"
  ) |>
  complete(
    indicador,
    ano_aprovacao = anos_educacao_tabela,
    fill = list(
      registros = 0,
      operacoes = 0,
      ufs = 0,
      valor_previsto = 0,
      valor_atingido = 0
    )
  )

tabela_educacao_resultados_por_ano <- educacao_aprovacao |>
  mutate(
    diferenca_atingido_previsto = valor_atingido - valor_previsto,
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    classe_cobertura = classificar_cobertura(cobertura)
  ) |>
  arrange(ano_aprovacao, indicador)

educacao_aprovacao_ano <- educacao_base_aprovacao |>
  group_by(ano_aprovacao) |>
  summarise(
    registros_indicadores = n(),
    indicadores_distintos = n_distinct(indicador),
    operacoes = n_distinct(numero_operacao),
    ufs = n_distinct(uf_analise),
    indicadores_com_previsto = sum(!is.na(valor_previsto)),
    indicadores_com_atingido = sum(realizado_indicador_disponivel),
    valor_previsto = sum(valor_previsto, na.rm = TRUE),
    valor_atingido = sum(valor_atingido_indicador, na.rm = TRUE),
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_),
    observacao = paste(
      "Valores previstos e atingidos somam indicadores com unidades diferentes;",
      "usar a tabela por indicador para interpretações de magnitude."
    ),
    .groups = "drop"
  ) |>
  complete(
    ano_aprovacao = anos_educacao_tabela,
    fill = list(
      registros_indicadores = 0,
      indicadores_distintos = 0,
      operacoes = 0,
      ufs = 0,
      indicadores_com_previsto = 0,
      indicadores_com_atingido = 0,
      valor_previsto = 0,
      valor_atingido = 0
    )
  ) |>
  mutate(
    cobertura = if_else(valor_previsto > 0, valor_atingido / valor_previsto, NA_real_)
  ) |>
  arrange(ano_aprovacao)

tabela_educacao_resultados <- educacao_aprovacao |>
  select(indicador, ano_aprovacao, valor_previsto, valor_atingido) |>
  pivot_wider(
    names_from = ano_aprovacao,
    values_from = c(valor_previsto, valor_atingido),
    values_fill = 0
  ) |>
  mutate(
    across(
      all_of(paste0("valor_previsto_", anos_educacao_tabela)),
      ~coalesce(.x, 0)
    ),
    across(
      all_of(paste0("valor_atingido_", anos_educacao_tabela)),
      ~coalesce(.x, 0)
    ),
    valor_previsto_total = rowSums(across(starts_with("valor_previsto_")), na.rm = TRUE),
    valor_atingido_total = rowSums(across(starts_with("valor_atingido_")), na.rm = TRUE)
  ) |>
  left_join(
    educacao_aprovacao |>
      group_by(indicador) |>
      summarise(
        alerta_unidade = primeiro_texto(alerta_unidade),
        .groups = "drop"
      ),
    by = "indicador"
  ) |>
  arrange(indicador)

notas_metodologicas <- tibble(
  tema = c(
    "Join",
    "Realizado dos indicadores",
    "Comparação financeira",
    "Totais por eixo",
    "Ano dos indicadores",
    "Unidade dos indicadores",
    "UF Interestadual"
  ),
  nota = c(
    paste(
      "O join foi realizado pelo número da operação após agregar os",
      "subcréditos da base FUST."
    ),
    paste(
      "A Base de QR FUST 2 contém a coluna valor.indicador.",
      "Nesta análise, ela foi tratada como valor atingido e comparada",
      "com operação QR indicador previsto valor (n)."
    ),
    paste(
      "Na análise financeira, previsto corresponde ao valor contratado e",
      "realizado corresponde ao valor desembolsado."
    ),
    paste(
      "Uma operação pode participar de mais de um eixo.",
      "Os totais financeiros dos eixos não devem ser somados entre si."
    ),
    paste(
      "Nas tabelas e gráficos por indicador, ano corresponde ao ano da data",
      "prevista do indicador. Quando essa data não existe, foi usado",
      "ano.aprovacao."
    ),
    paste(
      "Indicadores de contagem com valores fracionários positivos menores",
      "que 1 foram mantidos como informados na base e sinalizados para",
      "verificação de unidade."
    ),
    "IE foi apresentada como Interestadual (IE)."
  )
)

salvar_csv_qr(
  base_integrada,
  "2.1 Consolidação dos indicadores - base integrada QR e operações FUST.csv"
)
salvar_csv_qr(
  cobertura_join,
  "2.1 Consolidação dos indicadores - cobertura e qualidade do join.csv"
)
salvar_csv_qr(
  resumo_geral,
  "2.1 Consolidação dos indicadores - resumo por eixo e tipo.csv"
)
salvar_csv_qr(
  eficacia_eixo,
  "2.1.1 Eficácia - resumo por eixo.csv"
)
salvar_csv_qr(
  eficacia_indicador,
  "2.1.1 Eficácia - indicadores por eixo.csv"
)
salvar_csv_qr(
  eficacia_uf,
  "2.1.1 Eficácia - indicadores por UF e eixo.csv"
)
salvar_csv_qr(
  efetividade_eixo,
  "2.1.2 Efetividade - resumo por eixo.csv"
)
salvar_csv_qr(
  efetividade_indicador,
  "2.1.2 Efetividade - indicadores por eixo.csv"
)
salvar_csv_qr(
  efetividade_uf,
  "2.1.2 Efetividade - indicadores por UF e eixo.csv"
)
salvar_csv_qr(
  financeiro_eixo,
  "2.1 Previsto versus realizado financeiro - por eixo.csv"
)
salvar_csv_qr(
  financeiro_uf,
  "2.1 Previsto versus realizado financeiro - por UF.csv"
)
salvar_csv_qr(
  financeiro_uf_eixo,
  "2.1 Previsto versus realizado financeiro - por UF e eixo.csv"
)
salvar_csv_qr(
  disponibilidade_realizado,
  "2.1 Previsto versus realizado dos indicadores - disponibilidade na base.csv"
)
salvar_csv_qr(
  indicador_ano,
  "2.1 Previsto versus atingido dos indicadores - por ano.csv"
)
salvar_csv_qr(
  indicador_total,
  "2.1 Previsto versus atingido dos indicadores - cobertura por indicador.csv"
)
salvar_csv_qr(
  indicador_uf,
  "2.1 Previsto versus atingido dos indicadores - por UF.csv"
)
salvar_csv_qr(
  telecom_aprovacao_ano,
  paste0(titulo_tabela_telecom_texto, " - análise por ano.csv")
)
salvar_csv_qr(
  tabela_telecom_resultados_por_ano,
  paste0(titulo_tabela_telecom_texto, " - detalhamento por ano e indicador.csv")
)
salvar_csv_qr(
  tabela_telecom_resultados,
  paste0(titulo_tabela_telecom_texto, ".csv")
)
salvar_csv_qr(
  tabela_educacao_resultados,
  paste0(titulo_tabela_educacao_texto, ".csv")
)
salvar_csv_qr(
  educacao_aprovacao_ano,
  paste0(titulo_tabela_educacao_texto, " - resumo por ano.csv")
)
salvar_csv_qr(
  tabela_educacao_resultados_por_ano,
  paste0(titulo_tabela_educacao_texto, " - detalhamento por ano e indicador.csv")
)
abas_telecom_ano <- setNames(
  lapply(
    anos_telecom_tabela,
    function(ano) tabela_telecom_resultados_por_ano |> filter(ano_aprovacao == ano)
  ),
  as.character(anos_telecom_tabela)
)
abas_educacao_ano <- setNames(
  lapply(
    anos_educacao_tabela,
    function(ano) tabela_educacao_resultados_por_ano |> filter(ano_aprovacao == ano)
  ),
  as.character(anos_educacao_tabela)
)
write_xlsx(
  c(
    abas_telecom_ano,
    list(
    total = tabela_telecom_resultados,
    por_ano = tabela_telecom_resultados_por_ano,
    resumo_ano = telecom_aprovacao_ano
    )
  ),
  file.path(qr_out_dir, paste0(titulo_tabela_telecom_texto, ".xlsx"))
)
write_xlsx(
  c(
    abas_educacao_ano,
    list(
    total = tabela_educacao_resultados,
    por_ano = tabela_educacao_resultados_por_ano,
    resumo_ano = educacao_aprovacao_ano
    )
  ),
  file.path(qr_out_dir, paste0(titulo_tabela_educacao_texto, ".xlsx"))
)
salvar_tabelao_resultados_xlsx(
  list(
    valor_previsto = list(
      dados = tabelao_resultados_previsto,
      titulo = paste0(titulo_tabelao_resultados_texto, " - valor previsto"),
      subtitulo = "Níveis separados em Produtos e serviços (eficácia) e Objetivos específicos (efetividade). Colunas anuais usam ano_aprovacao da Base de QR FUST 2.",
      percentual = FALSE
    ),
    valor_atingido = list(
      dados = tabelao_resultados_atingido,
      titulo = paste0(titulo_tabelao_resultados_texto, " - valor atingido"),
      subtitulo = "Valor atingido corresponde à coluna valor.indicador tratada como realizado dos indicadores.",
      percentual = FALSE
    ),
    cobertura = list(
      dados = tabelao_resultados_cobertura,
      titulo = paste0(titulo_tabelao_resultados_texto, " - cobertura atingido/previsto"),
      subtitulo = "Cobertura calculada como valor atingido dividido pelo valor previsto. Células vazias indicam ausência de valor previsto positivo.",
      percentual = TRUE
    )
  ),
  file.path(
    qr_out_dir,
    "Quadro 1 - Indicadores de eficácia e efetividade das operações diretas.xlsx"
  )
)
salvar_csv_qr(
  notas_metodologicas,
  "2.1 Notas metodológicas.csv"
)

tabela_telecom_png <- tabela_telecom_resultados |>
  mutate(Indicador = str_wrap(indicador, width = 58)) |>
  transmute(
    Indicador,
    across(
      all_of(paste0("valor_previsto_", anos_telecom_tabela)),
      fmt_tabela_indicador
    ),
    total_previsto = fmt_tabela_indicador(valor_previsto_total),
    across(
      all_of(paste0("valor_atingido_", anos_telecom_tabela)),
      fmt_tabela_indicador
    ),
    total_atingido = fmt_tabela_indicador(valor_atingido_total),
    Obs. = if_else(is.na(alerta_unidade), "", "*")
  ) |>
  as.data.frame(check.names = FALSE)

names(tabela_telecom_png) <- c(
  "Indicador",
  as.character(anos_telecom_tabela),
  periodo_telecom,
  as.character(anos_telecom_tabela),
  periodo_telecom,
  "Obs."
)

tema_tabela_telecom <- ttheme_minimal(
  base_size = 9,
  core = list(
    fg_params = list(
      col = cor_texto,
      fontface = "bold",
      hjust = c(0, rep(0.5, ncol(tabela_telecom_png) - 1)),
      x = c(0.02, rep(0.5, ncol(tabela_telecom_png) - 1))
    ),
    bg_params = list(
      fill = rep(c("#FFFFFF", "#F4F7F6"), length.out = nrow(tabela_telecom_png)),
      col = "#B7B7B7",
      lwd = 0.6
    )
  ),
  colhead = list(
    fg_params = list(
      col = cor_azul,
      fontface = "bold",
      hjust = c(0, rep(0.5, ncol(tabela_telecom_png) - 1)),
      x = c(0.02, rep(0.5, ncol(tabela_telecom_png) - 1))
    ),
    bg_params = list(fill = "#EAF2F1", col = "#B7B7B7", lwd = 0.7)
  )
)

g_tabela_telecom <- tableGrob(
  tabela_telecom_png,
  rows = NULL,
  theme = tema_tabela_telecom
) |>
  alinhar_primeira_coluna()

titulo_tabela_telecom <- textGrob(
  titulo_tabela_telecom_texto,
  x = 0,
  hjust = 0,
  gp = gpar(col = cor_azul, fontface = "bold", fontsize = 11)
)

subtitulo_previsto_telecom <- textGrob(
  "Valor previsto",
  x = 0.51,
  hjust = 0.5,
  gp = gpar(col = cor_azul, fontface = "bold", fontsize = 10)
)

subtitulo_realizado_telecom <- textGrob(
  "Valor realizado",
  x = 0.78,
  hjust = 0.5,
  gp = gpar(col = cor_azul, fontface = "bold", fontsize = 10)
)

nota_tabela_telecom <- textGrob(
  paste(
    "* Verificar unidade: indicador de contagem com valor fracionário menor que 1 na base.",
    "Valores previstos e atingidos somam indicadores com unidades diferentes."
  ),
  x = 0,
  hjust = 0,
  gp = gpar(col = cor_texto, fontsize = 8)
)

arquivo_tabela_telecom <- file.path(
  qr_out_dir,
  paste0(titulo_tabela_telecom_texto, ".png")
)

png(
  filename = arquivo_tabela_telecom,
  width = 14,
  height = max(5.5, 0.42 * nrow(tabela_telecom_png) + 1.8),
  units = "in",
  res = 220,
  bg = "transparent"
)
grid.newpage()
grid.draw(
  arrangeGrob(
    titulo_tabela_telecom,
    gTree(children = gList(subtitulo_previsto_telecom, subtitulo_realizado_telecom)),
    g_tabela_telecom,
    nota_tabela_telecom,
    ncol = 1,
    heights = unit.c(unit(0.42, "in"), unit(0.25, "in"), unit(1, "null"), unit(0.28, "in"))
  )
)
dev.off()

tabela_educacao_png <- tabela_educacao_resultados |>
  mutate(Indicador = str_wrap(indicador, width = 58)) |>
  transmute(
    Indicador,
    across(
      all_of(paste0("valor_previsto_", anos_educacao_tabela)),
      fmt_tabela_indicador
    ),
    total_previsto = fmt_tabela_indicador(valor_previsto_total),
    across(
      all_of(paste0("valor_atingido_", anos_educacao_tabela)),
      fmt_tabela_indicador
    ),
    total_atingido = fmt_tabela_indicador(valor_atingido_total),
    Obs. = if_else(is.na(alerta_unidade), "", "*")
  ) |>
  as.data.frame(check.names = FALSE)

names(tabela_educacao_png) <- c(
  "Indicador",
  as.character(anos_educacao_tabela),
  periodo_educacao,
  as.character(anos_educacao_tabela),
  periodo_educacao,
  "Obs."
)

tema_tabela_educacao <- ttheme_minimal(
  base_size = 9,
  core = list(
    fg_params = list(
      col = cor_texto,
      fontface = "bold",
      hjust = c(0, rep(0.5, ncol(tabela_educacao_png) - 1)),
      x = c(0.02, rep(0.5, ncol(tabela_educacao_png) - 1))
    ),
    bg_params = list(
      fill = rep(c("#FFFFFF", "#F4F7F6"), length.out = nrow(tabela_educacao_png)),
      col = "#B7B7B7",
      lwd = 0.6
    )
  ),
  colhead = list(
    fg_params = list(
      col = cor_azul,
      fontface = "bold",
      hjust = c(0, rep(0.5, ncol(tabela_educacao_png) - 1)),
      x = c(0.02, rep(0.5, ncol(tabela_educacao_png) - 1))
    ),
    bg_params = list(fill = "#EAF2F1", col = "#B7B7B7", lwd = 0.7)
  )
)

g_tabela_educacao <- tableGrob(
  tabela_educacao_png,
  rows = NULL,
  theme = tema_tabela_educacao
) |>
  alinhar_primeira_coluna()

titulo_tabela_educacao <- textGrob(
  titulo_tabela_educacao_texto,
  x = 0,
  hjust = 0,
  gp = gpar(col = cor_azul, fontface = "bold", fontsize = 11)
)

subtitulo_previsto_educacao <- textGrob(
  "Valor previsto",
  x = 0.63,
  hjust = 0.5,
  gp = gpar(col = cor_azul, fontface = "bold", fontsize = 10)
)

fonte_tabela_educacao <- textGrob(
  "Fonte: Elaboração própria, com base na Base de QR FUST 2.",
  x = 0,
  hjust = 0,
  gp = gpar(col = cor_texto, fontsize = 8)
)

nota_tabela_educacao <- textGrob(
  str_wrap(
    paste(
      "Nota: valores previstos e atingidos somam indicadores com unidades diferentes;",
      "usar as abas por ano e por indicador do XLSX para interpretações de magnitude."
    ),
    width = 150
  ),
  x = 0,
  hjust = 0,
  gp = gpar(col = cor_texto, fontsize = 8)
)

arquivo_tabela_educacao <- file.path(
  qr_out_dir,
  paste0(titulo_tabela_educacao_texto, ".png")
)

png(
  filename = arquivo_tabela_educacao,
  width = 13.5,
  height = max(5.5, 0.42 * nrow(tabela_educacao_png) + 2.2),
  units = "in",
  res = 220,
  bg = "transparent"
)
grid.newpage()
grid.draw(
  arrangeGrob(
    titulo_tabela_educacao,
    subtitulo_previsto_educacao,
    g_tabela_educacao,
    fonte_tabela_educacao,
    nota_tabela_educacao,
    ncol = 1,
    heights = unit.c(
      unit(0.42, "in"),
      unit(0.25, "in"),
      unit(1, "null"),
      unit(0.22, "in"),
      unit(0.75, "in")
    )
  )
)
dev.off()

telecom_aprovacao_ano_plot <- telecom_aprovacao_ano |>
  select(
    ano_aprovacao,
    registros_indicadores,
    indicadores_distintos,
    operacoes
  ) |>
  pivot_longer(
    c(registros_indicadores, indicadores_distintos, operacoes),
    names_to = "metrica",
    values_to = "valor"
  ) |>
  mutate(
    ano_aprovacao = factor(ano_aprovacao),
    metrica = factor(
      metrica,
      levels = c(
        "registros_indicadores",
        "indicadores_distintos",
        "operacoes"
      ),
      labels = c(
        "Registros de indicadores",
        "Indicadores distintos",
        "Operações"
      )
    )
  )

p_telecom_aprovacao_ano <- ggplot(
  telecom_aprovacao_ano_plot,
  aes(x = ano_aprovacao, y = valor, fill = metrica)
) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  geom_text(
    aes(label = fmt_int(valor)),
    position = position_dodge(width = 0.76),
    vjust = -0.45,
    color = "black",
    fontface = "bold",
    size = 3.2
  ) +
  scale_fill_manual(values = c(cor_azul, cor_verde, cor_azul_claro)) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.16))
  ) +
  labs(x = NULL, y = NULL, fill = NULL) +
  tema_fust_escuro()

salvar_figura_qr(
  p_telecom_aprovacao_ano,
  paste0(titulo_tabela_telecom_texto, " - análise por ano.png"),
  width = 8.5,
  height = 4.8
)

portfolio_plot <- resumo_geral |>
  transmute(
    tipo_indicador,
    eixo = str_wrap(eixo, width = 24),
    registros_indicadores
  )

p_portfolio <- ggplot(
  portfolio_plot,
  aes(x = eixo, y = registros_indicadores, fill = tipo_indicador)
) +
  geom_col(position = position_dodge(width = 0.72), width = 0.62) +
  geom_text(
    aes(label = fmt_int(registros_indicadores)),
    position = position_dodge(width = 0.72),
    vjust = -0.5,
    color = "black",
    fontface = "bold",
    size = 3.4
  ) +
  scale_fill_manual(
    values = c("Eficácia" = cor_azul, "Efetividade" = cor_verde, "Misto" = cor_azul_claro)
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro() +
  theme(legend.position = "top")

salvar_figura_qr(
  p_portfolio,
  "2.1 Consolidação dos indicadores - quantidade por eixo e tipo.png",
  width = 9.5,
  height = 5.5
)

plotar_tipo_eixo <- function(tipo, resumo, prefixo) {
  dados <- resumo |>
    select(eixo, registros_indicadores, indicadores_distintos, operacoes) |>
    pivot_longer(
      c(registros_indicadores, indicadores_distintos, operacoes),
      names_to = "metrica",
      values_to = "valor"
    ) |>
    mutate(
      eixo = str_wrap(eixo, width = 24),
      metrica = factor(
        metrica,
        levels = c(
          "registros_indicadores",
          "indicadores_distintos",
          "operacoes"
        ),
        labels = c(
          "Registros de indicadores",
          "Indicadores distintos",
          "Operações"
        )
      )
    )

  p <- ggplot(dados, aes(x = eixo, y = valor, fill = metrica)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68) +
    geom_text(
      aes(label = fmt_int(valor)),
      position = position_dodge(width = 0.78),
      vjust = -0.5,
      color = "black",
      fontface = "bold",
      size = 3.1
    ) +
    scale_fill_manual(
      values = c(cor_azul, cor_verde, cor_azul_claro)
    ) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0, 0.15))
    ) +
    labs(fill = NULL) +
    tema_fust_escuro()

  salvar_figura_qr(
    p,
    paste0(prefixo, " - indicadores e operações por eixo.png"),
    width = 9.5,
    height = 5.5
  )
}

plotar_tipo_eixo(
  "Eficácia",
  eficacia_eixo,
  "2.1.1 Eficácia"
)
plotar_tipo_eixo(
  "Efetividade",
  efetividade_eixo,
  "2.1.2 Efetividade"
)

uf_plot <- base_integrada |>
  count(uf_analise, tipo_indicador, name = "indicadores") |>
  mutate(
    uf_analise = factor(
      uf_analise,
      levels = rev(unique(financeiro_uf$uf_analise))
    )
  )

p_uf_indicadores <- ggplot(
  uf_plot,
  aes(x = uf_analise, y = indicadores, fill = tipo_indicador)
) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = if_else(indicadores > 0, fmt_int(indicadores), "")),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 3.1
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Eficácia" = cor_azul, "Efetividade" = cor_verde)
  ) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = cor_grade, linewidth = 0.75)
  )

salvar_figura_qr(
  p_uf_indicadores,
  "2.1 Análise por UF - indicadores de eficácia e efetividade.png",
  width = 9.5,
  height = 6.5
)

financeiro_eixo_plot <- financeiro_eixo |>
  select(
    eixo,
    valor_previsto_financeiro,
    valor_realizado_financeiro
  ) |>
  pivot_longer(
    c(valor_previsto_financeiro, valor_realizado_financeiro),
    names_to = "situacao",
    values_to = "valor"
  ) |>
  mutate(
    eixo = str_wrap(eixo, width = 24),
    situacao = factor(
      situacao,
      levels = c(
        "valor_previsto_financeiro",
        "valor_realizado_financeiro"
      ),
      labels = c("Previsto: contratado", "Realizado: desembolsado")
    )
  )

p_financeiro_eixo <- ggplot(
  financeiro_eixo_plot,
  aes(x = eixo, y = valor, fill = situacao)
) +
  geom_col(position = position_dodge(width = 0.76), width = 0.66) +
  geom_text(
    aes(label = fmt_dinheiro_curto(valor)),
    position = position_dodge(width = 0.76),
    vjust = -0.5,
    color = "black",
    fontface = "bold",
    size = 3
  ) +
  scale_fill_manual(values = c(cor_azul, cor_verde)) +
  scale_y_continuous(
    labels = fmt_milhoes_eixo,
    expand = expansion(mult = c(0, 0.14))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro()

salvar_figura_qr(
  p_financeiro_eixo,
  "2.1 Previsto versus realizado financeiro - comparação por eixo.png",
  width = 11,
  height = 5.8
)

financeiro_uf_plot <- financeiro_uf |>
  select(
    uf_analise,
    valor_previsto_financeiro,
    valor_realizado_financeiro
  ) |>
  pivot_longer(
    c(valor_previsto_financeiro, valor_realizado_financeiro),
    names_to = "situacao",
    values_to = "valor"
  ) |>
  mutate(
    uf_analise = factor(
      uf_analise,
      levels = rev(financeiro_uf$uf_analise)
    ),
    situacao = factor(
      situacao,
      levels = c(
        "valor_previsto_financeiro",
        "valor_realizado_financeiro"
      ),
      labels = c("Previsto: contratado", "Realizado: desembolsado")
    )
  )

p_financeiro_uf <- ggplot(
  financeiro_uf_plot,
  aes(x = uf_analise, y = valor, fill = situacao)
) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = fmt_dinheiro_curto(valor)),
    position = position_dodge(width = 0.75),
    hjust = -0.08,
    color = "black",
    fontface = "bold",
    size = 2.8
  ) +
  coord_flip() +
  scale_fill_manual(values = c(cor_azul, cor_verde)) +
  scale_y_continuous(
    labels = fmt_milhoes_eixo,
    expand = expansion(mult = c(0, 0.2))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = cor_grade, linewidth = 0.75)
  )

salvar_figura_qr(
  p_financeiro_uf,
  "2.1 Previsto versus realizado financeiro - comparação por UF.png",
  width = 10,
  height = 7
)

ordem_indicadores_cobertura <- indicador_total |>
  arrange(cobertura, tipo_indicador, indicador) |>
  mutate(indicador_plot = paste(indicador, tipo_indicador, eixo, sep = " | ")) |>
  pull(indicador_plot)

cobertura_indicador_plot <- indicador_total |>
  mutate(
    indicador_plot = paste(indicador, tipo_indicador, eixo, sep = " | "),
    indicador_plot = factor(indicador_plot, levels = rev(ordem_indicadores_cobertura)),
    cobertura_label = if_else(!is.na(cobertura) & cobertura < 0.999, fmt_pct_ratio(cobertura), "")
  )

p_cobertura_indicador <- ggplot(
  cobertura_indicador_plot,
  aes(x = indicador_plot, y = cobertura, fill = tipo_indicador)
) +
  geom_col(width = 0.68) +
  geom_hline(yintercept = 1, linewidth = 0.8, color = cor_verde_escuro) +
  geom_text(
    aes(label = cobertura_label),
    hjust = -0.08,
    color = "black",
    fontface = "bold",
    size = 2.7
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("Eficácia" = cor_azul, "Efetividade" = cor_verde)
  ) +
  scale_y_continuous(
    labels = function(x) fmt_pct_ratio(x),
    expand = expansion(mult = c(0, 0.2))
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 34)) +
  labs(fill = "Tipo de indicador") +
  tema_fust_escuro() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = cor_grade, linewidth = 0.75),
    axis.text.y = element_text(size = 8.2),
    plot.margin = margin(18, 78, 18, 16)
  )

salvar_figura_qr(
  p_cobertura_indicador,
  "2.1 Previsto versus atingido dos indicadores - cobertura por indicador.png",
  width = 11,
  height = 9.5
)

cobertura_ano_plot <- indicador_ano |>
  mutate(
    indicador_plot = paste(indicador, tipo_indicador, eixo, sep = " | "),
    indicador_plot = factor(indicador_plot, levels = rev(ordem_indicadores_cobertura)),
    ano_indicador = factor(ano_indicador),
    classe_cobertura = factor(
      classe_cobertura,
      levels = c(
        "Abaixo de 50%",
        "50% a 79%",
        "80% a 99%",
        "100% ou mais",
        "Sem previsto"
      )
    ),
    cobertura_label = if_else(!is.na(cobertura) & cobertura < 0.999, fmt_pct_ratio(cobertura), "")
  )

p_cobertura_ano <- ggplot(
  cobertura_ano_plot,
  aes(x = ano_indicador, y = indicador_plot, fill = classe_cobertura)
) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(
    aes(label = cobertura_label),
    color = "black",
    fontface = "bold",
    size = 2.35
  ) +
  scale_fill_manual(
    values = c(
      "Abaixo de 50%" = cor_azul_escuro,
      "50% a 79%" = cor_azul,
      "80% a 99%" = cor_verde_claro,
      "100% ou mais" = cor_verde,
      "Sem previsto" = cor_azul_claro
    ),
    drop = FALSE
  ) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 32)) +
  labs(fill = "Cobertura") +
  tema_fust_escuro() +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8.1)
  )

salvar_figura_qr(
  p_cobertura_ano,
  "2.1 Previsto versus atingido dos indicadores - cobertura por ano.png",
  width = 10.5,
  height = 9.5
)

rotulos_indicadores_cobertura <- indicador_ano |>
  distinct(indicador, tipo_indicador, eixo) |>
  mutate(
    indicador_curto = sintetizar_indicador(indicador),
    indicador_curto = if_else(
      nchar(indicador_curto) > 68,
      str_trunc(indicador_curto, width = 68, side = "right", ellipsis = "..."),
      indicador_curto
    ),
    indicador_curto = make.unique(indicador_curto, sep = " #")
  ) |>
  arrange(tipo_indicador, eixo, indicador_curto)

salvar_csv_qr(
  rotulos_indicadores_cobertura,
  "2.1 Previsto versus atingido dos indicadores - rotulos sintetizados.csv"
)

anos_cobertura_indicador <- sort(unique(indicador_ano$ano_indicador[!is.na(indicador_ano$ano_indicador)]))

cobertura_ano_limpa_base <- indicador_ano |>
  distinct(indicador, tipo_indicador, eixo) |>
  left_join(
    rotulos_indicadores_cobertura,
    by = c("indicador", "tipo_indicador", "eixo")
  ) |>
  mutate(
    tipo_indicador_norm = normalizar_texto(tipo_indicador),
    nivel_resultado = case_when(
      tipo_indicador_norm == "EFICACIA" ~ "Eficácia",
      tipo_indicador_norm == "EFETIVIDADE" ~ "Efetividade",
      TRUE ~ tipo_indicador
    ),
    eixo_curto = case_when(
      eixo == "TICs - Telecom e data center" ~ "Telecom e data center",
      TRUE ~ eixo
    ),
    ordem_nivel = case_when(
      nivel_resultado == "Eficácia" ~ 1L,
      nivel_resultado == "Efetividade" ~ 2L,
      TRUE ~ 3L
    ),
    painel = paste(nivel_resultado, eixo_curto, sep = " | ")
  )

cobertura_ano_limpa <- cobertura_ano_limpa_base |>
  crossing(ano_indicador = anos_cobertura_indicador) |>
  left_join(
    indicador_ano |>
      select(
        indicador,
        tipo_indicador,
        eixo,
        ano_indicador,
        cobertura,
        classe_cobertura
      ),
    by = c("indicador", "tipo_indicador", "eixo", "ano_indicador")
  ) |>
  mutate(
    ano_indicador = factor(ano_indicador),
    classe_cobertura_plot = if_else(
      is.na(classe_cobertura),
      "Sem registro",
      as.character(classe_cobertura)
    ),
    classe_cobertura_plot = factor(
      classe_cobertura_plot,
      levels = c(
        "Abaixo de 50%",
        "50% a 79%",
        "80% a 99%",
        "100% ou mais",
        "Sem previsto",
        "Sem registro"
      )
    ),
    cobertura_label = case_when(
      is.na(cobertura) ~ "",
      cobertura < 0.999 ~ fmt_pct_ratio(cobertura),
      TRUE ~ ""
    )
  ) |>
  group_by(painel) |>
  mutate(
    ordem_indicador = min(coalesce(cobertura, Inf), na.rm = TRUE),
    indicador_curto = factor(
      indicador_curto,
      levels = rev(unique(indicador_curto[order(ordem_indicador, indicador_curto)]))
    )
  ) |>
  ungroup() |>
  arrange(ordem_nivel, eixo_curto, indicador_curto) |>
  mutate(
    painel = factor(painel, levels = unique(painel[order(ordem_nivel, eixo_curto)])),
    classe_cobertura_limpa = case_when(
      is.na(cobertura) ~ "Sem registro",
      cobertura < 0.999 ~ "Abaixo de 100%",
      TRUE ~ "100% ou mais"
    ),
    classe_cobertura_limpa = factor(
      classe_cobertura_limpa,
      levels = c("Abaixo de 100%", "100% ou mais", "Sem registro")
    )
  )

p_cobertura_ano_limpa <- ggplot(
  cobertura_ano_limpa,
  aes(x = ano_indicador, y = indicador_curto, fill = classe_cobertura_limpa)
) +
  geom_tile(color = "#FFFFFF", linewidth = 0.8) +
  geom_text(
    aes(label = cobertura_label),
    color = "#111111",
    fontface = "bold",
    size = 2.7
  ) +
  facet_grid(
    painel ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_fill_manual(
    values = c(
      "Abaixo de 100%" = "#D73027",
      "100% ou mais" = cor_verde,
      "Sem registro" = "#F2F2F2"
    ),
    drop = FALSE
  ) +
  labs(x = NULL, y = NULL, fill = "Cobertura") +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#222222", face = "bold", size = 10),
    axis.text.y = element_text(color = cor_verde, face = "bold", size = 8.2),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      color = cor_azul,
      face = "bold",
      angle = 0,
      hjust = 0
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(0.32, "in"),
    plot.margin = margin(12, 18, 12, 18)
  )

altura_heatmap_limpo <- max(
  7,
  0.28 * n_distinct(cobertura_ano_limpa$indicador_curto) +
    0.55 * n_distinct(cobertura_ano_limpa$painel) +
    1.8
)

salvar_figura_qr(
  p_cobertura_ano_limpa,
  "2.1 Previsto versus atingido dos indicadores - heatmap cobertura limpo.png",
  width = 11,
  height = altura_heatmap_limpo
)

p_cobertura_ano_limpa_transparente <- p_cobertura_ano_limpa +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    strip.background = element_rect(fill = "transparent", color = NA)
  )

salvar_figura_qr(
  p_cobertura_ano_limpa_transparente,
  "2.1 Previsto versus atingido dos indicadores - heatmap cobertura limpo transparente.png",
  width = 11,
  height = altura_heatmap_limpo
)

legenda_heatmap_compacto <- cobertura_ano_limpa |>
  distinct(
    painel,
    ordem_nivel,
    eixo_curto,
    indicador_curto,
    indicador,
    tipo_indicador,
    eixo
  ) |>
  arrange(ordem_nivel, eixo_curto, indicador_curto) |>
  mutate(
    codigo_indicador = paste0("I", str_pad(row_number(), width = 2, pad = "0"))
  ) |>
  select(
    codigo_indicador,
    painel,
    indicador_curto,
    indicador_original = indicador,
    tipo_indicador,
    eixo
  )

salvar_csv_qr(
  legenda_heatmap_compacto,
  "2.1 Previsto versus atingido dos indicadores - legenda heatmap compacto.csv"
)

write_xlsx(
  list(
    legenda = legenda_heatmap_compacto,
    dados_heatmap = cobertura_ano_limpa |>
      left_join(
        legenda_heatmap_compacto |>
          transmute(
            codigo_indicador,
            indicador = indicador_original,
            tipo_indicador,
            eixo
          ),
        by = c("indicador", "tipo_indicador", "eixo")
      ) |>
      mutate(
        classe_compacta = case_when(
          is.na(cobertura) ~ "Sem registro",
          cobertura < 0.999 ~ "Abaixo de 100%",
          TRUE ~ "100% ou mais"
        )
      ) |>
      select(
        codigo_indicador,
        painel,
        ano_indicador,
        classe_cobertura = classe_cobertura_plot,
        classe_compacta,
        cobertura,
        indicador_curto,
        indicador
      )
  ),
  file.path(
    qr_out_dir,
    "2.1 Previsto versus atingido dos indicadores - legenda heatmap compacto.xlsx"
  )
)

cobertura_ano_compacta <- cobertura_ano_limpa |>
  left_join(
    legenda_heatmap_compacto |>
      transmute(
        codigo_indicador,
        indicador = indicador_original,
        tipo_indicador,
        eixo
      ),
    by = c("indicador", "tipo_indicador", "eixo")
  ) |>
  mutate(
    classe_cobertura_compacta = case_when(
      is.na(cobertura) ~ "Sem registro",
      cobertura < 0.999 ~ "Abaixo de 100%",
      TRUE ~ "100% ou mais"
    ),
    classe_cobertura_compacta = factor(
      classe_cobertura_compacta,
      levels = c("Abaixo de 100%", "100% ou mais", "Sem registro")
    )
  ) |>
  group_by(painel) |>
  mutate(
    codigo_indicador = factor(
      codigo_indicador,
      levels = rev(unique(codigo_indicador))
    )
  ) |>
  ungroup()

p_cobertura_ano_compacta <- ggplot(
  cobertura_ano_compacta,
  aes(x = ano_indicador, y = codigo_indicador, fill = classe_cobertura_compacta)
) +
  geom_tile(color = "#FFFFFF", linewidth = 0.9) +
  geom_text(
    aes(label = cobertura_label),
    color = "#111111",
    fontface = "bold",
    size = 3
  ) +
  facet_grid(
    painel ~ .,
    scales = "free_y",
    space = "free_y",
    switch = "y"
  ) +
  scale_fill_manual(
    values = c(
      "Abaixo de 100%" = "#D73027",
      "100% ou mais" = "#1A9850",
      "Sem registro" = "#F2F2F2"
    ),
    drop = FALSE
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = "Cobertura"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "#222222", face = "bold", size = 10),
    axis.text.y = element_text(color = "#222222", face = "bold", size = 8.5),
    strip.placement = "outside",
    strip.text.y.left = element_text(
      color = cor_azul,
      face = "bold",
      angle = 0,
      hjust = 0
    ),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = unit(0.32, "in"),
    plot.margin = margin(12, 18, 12, 18)
  )

altura_heatmap_compacto <- max(
  6.5,
  0.24 * n_distinct(cobertura_ano_compacta$codigo_indicador) +
    0.5 * n_distinct(cobertura_ano_compacta$painel) +
    1.8
)

salvar_figura_qr(
  p_cobertura_ano_compacta,
  "2.1 Previsto versus atingido dos indicadores - heatmap cobertura compacto.png",
  width = 8.5,
  height = altura_heatmap_compacto
)

p_cobertura_ano_compacta_transparente <- p_cobertura_ano_compacta +
  theme(
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    strip.background = element_rect(fill = "transparent", color = NA)
  )

salvar_figura_qr(
  p_cobertura_ano_compacta_transparente,
  "2.1 Previsto versus atingido dos indicadores - heatmap cobertura compacto transparente.png",
  width = 8.5,
  height = altura_heatmap_compacto
)

indicadores_unicos <- indicador_ano |>
  distinct(indicador) |>
  arrange(indicador) |>
  mutate(indice = row_number())

for (i in seq_len(nrow(indicadores_unicos))) {
  indicador_atual <- indicadores_unicos$indicador[[i]]
  indice_atual <- indicadores_unicos$indice[[i]]

  dados_indicador <- indicador_ano |>
    filter(indicador == indicador_atual) |>
    select(
      ano_indicador,
      valor_previsto,
      valor_atingido,
      cobertura,
      alerta_unidade
    ) |>
    pivot_longer(
      c(valor_previsto, valor_atingido),
      names_to = "situacao",
      values_to = "valor"
    ) |>
    mutate(
      ano_indicador = factor(ano_indicador),
      situacao = factor(
        situacao,
        levels = c("valor_previsto", "valor_atingido"),
        labels = c("Previsto", "Atingido")
      ),
      label_valor = if_else(
        valor > 0,
        paste0(fmt_valor_indicador(valor), if_else(is.na(alerta_unidade), "", "*")),
        ""
      ),
      label_cobertura = if_else(
        situacao == "Atingido" & !is.na(cobertura) & cobertura < 0.999,
        paste0("Cob. ", fmt_pct_ratio(cobertura)),
        ""
      )
    )

  p_indicador <- ggplot(
    dados_indicador,
    aes(x = ano_indicador, y = valor, fill = situacao)
  ) +
    geom_col(position = position_dodge(width = 0.74), width = 0.64) +
    geom_text(
      aes(label = label_valor),
      position = position_dodge(width = 0.74),
      vjust = -0.45,
      color = "black",
      fontface = "bold",
      size = 3
    ) +
    geom_text(
      aes(label = label_cobertura),
      position = position_dodge(width = 0.74),
      vjust = 1.4,
      color = "black",
      fontface = "bold",
      size = 2.6
    ) +
    scale_fill_manual(values = c("Previsto" = cor_azul, "Atingido" = cor_verde)) +
    scale_y_continuous(
      labels = fmt_valor_indicador,
      expand = expansion(mult = c(0.02, 0.2))
    ) +
    labs(x = NULL, y = NULL, fill = "Situação") +
    tema_fust_escuro()

  salvar_figura_indicador(
    p_indicador,
    nome_arquivo_indicador(indicador_atual, indice_atual)
  )
}

indicadores_uf_selecionados <- indicador_uf |>
  group_by(indicador) |>
  summarise(
    ufs = n_distinct(uf_analise),
    registros = sum(registros, na.rm = TRUE),
    valor_previsto_total = sum(valor_previsto, na.rm = TRUE),
    valor_atingido_total = sum(valor_atingido, na.rm = TRUE),
    cobertura_total = if_else(
      valor_previsto_total > 0,
      valor_atingido_total / valor_previsto_total,
      NA_real_
    ),
    tem_gap = any(cobertura < 0.999, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(ufs > 1) |>
  arrange(desc(tem_gap), desc(ufs), desc(registros), indicador) |>
  slice_head(n = 8) |>
  mutate(indice = row_number())

for (i in seq_len(nrow(indicadores_uf_selecionados))) {
  indicador_atual <- indicadores_uf_selecionados$indicador[[i]]
  indice_atual <- indicadores_uf_selecionados$indice[[i]]

  dados_uf <- indicador_uf |>
    filter(indicador == indicador_atual) |>
    mutate(total_uf = valor_previsto + valor_atingido) |>
    arrange(total_uf) |>
    mutate(uf_analise = factor(uf_analise, levels = unique(uf_analise))) |>
    select(
      uf_analise,
      valor_previsto,
      valor_atingido,
      cobertura,
      alerta_unidade
    ) |>
    pivot_longer(
      c(valor_previsto, valor_atingido),
      names_to = "situacao",
      values_to = "valor"
    ) |>
    mutate(
      situacao = factor(
        situacao,
        levels = c("valor_previsto", "valor_atingido"),
        labels = c("Previsto", "Atingido")
      ),
      label_valor = if_else(
        valor > 0,
        paste0(fmt_valor_indicador(valor), if_else(is.na(alerta_unidade), "", "*")),
        ""
      ),
      label_cobertura = if_else(
        situacao == "Atingido" & !is.na(cobertura) & cobertura < 0.999,
        paste0("Cob. ", fmt_pct_ratio(cobertura)),
        ""
      )
    )

  altura_uf <- max(4.8, 0.45 * n_distinct(dados_uf$uf_analise) + 2.4)

  p_uf <- ggplot(
    dados_uf,
    aes(x = uf_analise, y = valor, fill = situacao)
  ) +
    geom_col(position = position_dodge(width = 0.74), width = 0.64) +
    geom_text(
      aes(label = label_valor),
      position = position_dodge(width = 0.74),
      hjust = -0.08,
      color = "black",
      fontface = "bold",
      size = 2.8
    ) +
    geom_text(
      aes(label = label_cobertura),
      position = position_dodge(width = 0.74),
      hjust = 1.05,
      color = "black",
      fontface = "bold",
      size = 2.45
    ) +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c("Previsto" = cor_azul, "Atingido" = cor_verde)) +
    scale_y_continuous(
      labels = fmt_valor_indicador,
      expand = expansion(mult = c(0, 0.25))
    ) +
    labs(x = NULL, y = NULL, fill = "Situação") +
    tema_fust_escuro() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = cor_grade, linewidth = 0.75),
      plot.margin = margin(18, 80, 18, 16)
    )

  salvar_figura_indicador_uf(
    p_uf,
    nome_arquivo_indicador(indicador_atual, indice_atual),
    height = altura_uf
  )
}

write_xlsx(
  list(
    base_integrada = base_integrada,
    cobertura_join = cobertura_join,
    resumo_geral = resumo_geral,
    eficacia_eixo = eficacia_eixo,
    eficacia_indicador = eficacia_indicador,
    eficacia_uf = eficacia_uf,
    efetividade_eixo = efetividade_eixo,
    efetividade_indicador = efetividade_indicador,
    efetividade_uf = efetividade_uf,
    financeiro_eixo = financeiro_eixo,
    financeiro_uf = financeiro_uf,
    financeiro_uf_eixo = financeiro_uf_eixo,
    disponibilidade_realizado = disponibilidade_realizado,
    indicador_ano = indicador_ano,
    indicador_total = indicador_total,
    indicador_uf = indicador_uf,
    tabelao_previsto = tabelao_resultados_previsto,
    tabelao_atingido = tabelao_resultados_atingido,
    tabelao_cobertura = tabelao_resultados_cobertura,
    telecom_aprovacao_ano = telecom_aprovacao_ano,
    telecom_resultados_por_ano = tabela_telecom_resultados_por_ano,
    tabela_telecom_resultados = tabela_telecom_resultados,
    educacao_aprovacao_ano = educacao_aprovacao_ano,
    educacao_resultados_por_ano = tabela_educacao_resultados_por_ano,
    tabela_educacao_resultados = tabela_educacao_resultados,
    notas_metodologicas = notas_metodologicas
  ),
  file.path(
    qr_out_dir,
    "2.1 Consolidação e análise do Quadro de Resultados.xlsx"
  )
)
