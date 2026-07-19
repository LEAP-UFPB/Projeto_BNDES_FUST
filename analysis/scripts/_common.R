pacotes_base <- c(
  "dplyr",
  "ggplot2",
  "grid",
  "readr",
  "readxl",
  "scales",
  "stringr",
  "tidyr"
)

faltando <- setdiff(pacotes_base, rownames(installed.packages()))
if (length(faltando) > 0) {
  install.packages(faltando)
}

suppressPackageStartupMessages({
  invisible(lapply(pacotes_base, library, character.only = TRUE))
})

cor_verde <- "#127D3E"
cor_azul <- "#2A4089"
cor_fundo <- "#000000"
cor_grade <- "#000000"
cor_texto <- "#000000"
cor_verde_claro <- "#67B884"
cor_azul_claro <- "#8FA3D9"
cor_verde_escuro <- "#0B4F27"
cor_azul_escuro <- "#182757"

paleta_fust <- c(
  cor_azul,
  cor_verde,
  cor_azul_claro,
  cor_verde_claro,
  cor_azul_escuro,
  cor_verde_escuro
)

paleta_origem <- c(
  "Direta" = cor_azul,
  "Indireta" = cor_verde
)

ufs_brasil <- c(
  "AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO",
  "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI",
  "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"
)

args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grepl("^--file=", args)])
if (length(script_path) == 0 || script_path == "") {
  script_path <- file.path(getwd(), "analysis", "scripts", "_common.R")
}
script_dir <- dirname(normalizePath(script_path))
analysis_dir <- dirname(script_dir)
root_dir <- dirname(analysis_dir)
data_dir <- file.path(root_dir, "data")
output_dir <- file.path(analysis_dir, "outputs")

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

arquivo_base <- file.path(data_dir, "Base de Operações_FUST_compilada.xlsx")

nomes_secao <- c(
  fig_valor_contratado_por_ano.png =
    "1.1 Evolução anual dos valores contratados e número de operações - valor contratado por ano.png",
  fig_operacao_vs_desembolso_por_ano.png =
    "1.1 Evolução anual dos valores contratados e número de operações - valor desembolsado por ano.png",
  fig_contratos_por_ano.png =
    "1.1 Evolução anual dos valores contratados e número de operações - número de operações por ano.png",
  fig_contratos_unicos_por_ano.png =
    "1.1 Evolucao anual dos valores contratados e numero de operacoes - numero de contratos unicos por ano.png",
  tab_resumo_anual.csv =
    "1.1 Evolução anual dos valores contratados e número de operações - tabela anual.csv",
  tab_comparativo_operacao_desembolso.csv =
    "1.1 Evolução anual dos valores contratados e número de operações - contratado desembolsado e operações.csv",

  fig_composicao_por_origem.png =
    "1.2 Desempenho por modalidade do fundo ou fonte de recurso - composição por modalidade.png",
  fig_composicao_por_origem_contratos_unicos.png =
    "1.2 Desempenho por modalidade do fundo ou fonte de recurso - composicao por modalidade - contratos unicos.png",
  fig_modalidade_contratado_vs_desembolsado.png =
    "1.2 Desempenho por modalidade do fundo ou fonte de recurso - contratado versus desembolsado.png",
  tab_ano_origem.csv =
    "1.2 Desempenho por modalidade do fundo ou fonte de recurso - tabela anual.csv",
  tab_comparativo_origem.csv =
    "1.2 Desempenho por modalidade do fundo ou fonte de recurso - comparativo.csv",

  fig_top_setor_cnae_valor_contratado.png =
    "1.3 Desempenho por setor CNAE dos clientes - valor contratado por subsetor CNAE.png",
  fig_top_setor_cnae_valor_desembolsado.png =
    "1.3 Desempenho por setor CNAE dos clientes - valor desembolsado por subsetor CNAE.png",
  fig_top_setor_cnae_operacoes.png =
    "1.3 Desempenho por setor CNAE dos clientes - número de operações por subsetor CNAE.png",
  fig_top_setor_cnae_contratos_unicos.png =
    "1.3 Desempenho por setor CNAE dos clientes - numero de contratos unicos por subsetor CNAE.png",
  tab_ano_setor_cnae.csv =
    "1.3 Desempenho por setor CNAE dos clientes - tabela anual.csv",
  tab_comparativo_setor_cnae.csv =
    "1.3 Desempenho por setor CNAE dos clientes - comparativo.csv",
  tab_top_setor_cnae.csv =
    "1.3 Desempenho por setor CNAE dos clientes - ranking.csv",

  fig_composicao_por_porte.png =
    "1.4 Desempenho por porte dos clientes - número de operações por porte.png",
  fig_composicao_por_porte_contratos_unicos.png =
    "1.4 Desempenho por porte dos clientes - numero de contratos unicos por porte.png",
  fig_composicao_por_porte_valor_contratado.png =
    "1.4 Desempenho por porte dos clientes - valor contratado por porte.png",
  fig_composicao_por_porte_valor_desembolsado.png =
    "1.4 Desempenho por porte dos clientes - valor desembolsado por porte.png",
  tab_ano_porte.csv =
    "1.4 Desempenho por porte dos clientes - tabela anual.csv",
  tab_comparativo_porte.csv =
    "1.4 Desempenho por porte dos clientes - comparativo.csv",

  fig_top_uf_valor_contratado.png =
    "1.5 Distribuição regional e por Unidade da Federação - valor contratado por UF.png",
  fig_top_uf_valor_desembolsado.png =
    "1.5 Distribuição regional e por Unidade da Federação - valor desembolsado por UF.png",
  fig_top_uf_operacoes.png =
    "1.5 Distribuição regional e por Unidade da Federação - número de operações por UF.png",
  fig_top_uf_contratos_unicos.png =
    "1.5 Distribuicao regional e por Unidade da Federacao - numero de contratos unicos por UF.png",
  fig_uf_todas_valor_contratado.png =
    "1.5 Distribuição regional e por Unidade da Federação - valor contratado por UF - todas as UFs.png",
  fig_uf_todas_valor_desembolsado.png =
    "1.5 Distribuição regional e por Unidade da Federação - valor desembolsado por UF - todas as UFs.png",
  fig_uf_todas_operacoes.png =
    "1.5 Distribuição regional e por Unidade da Federação - número de operações por UF - todas as UFs.png",
  fig_uf_todas_contratos_unicos.png =
    "1.5 Distribuicao regional e por Unidade da Federacao - numero de contratos unicos por UF - todas as UFs.png",
  map_valor_contratado_por_uf.png =
    "1.5 Distribuição regional e por Unidade da Federação - mapa valor contratado por UF.png",
  tab_ano_uf.csv =
    "1.5 Distribuição regional e por Unidade da Federação - tabela anual.csv",
  tab_comparativo_uf.csv =
    "1.5 Distribuição regional e por Unidade da Federação - comparativo.csv",
  tab_top_uf.csv =
    "1.5 Distribuição regional e por Unidade da Federação - ranking.csv",

  tab_faixas_juros.csv =
    "1.6 Distribuição por faixa de taxa de juros e prazo total - tabela.csv"
)

salvar_copia_secao <- function(nome) {
  if (!nome %in% names(nomes_secao)) {
    return(invisible(FALSE))
  }

  origem <- file.path(output_dir, nome)
  destino <- file.path(output_dir, nomes_secao[[nome]])
  if (file.exists(origem)) {
    file.copy(origem, destino, overwrite = TRUE)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}

normalizar_texto <- function(x) {
  x |>
    as.character() |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish()
}

numero_br <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  x <- as.character(x) |>
    str_squish() |>
    na_if("") |>
    na_if("ND")

  tem_virgula <- str_detect(x, ",")
  x <- if_else(
    tem_virgula,
    str_replace_all(str_replace_all(x, "\\.", ""), ",", "."),
    x
  )

  parse_number(x, locale = locale(decimal_mark = "."))
}

data_excel <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  x_chr <- as.character(x)
  serial <- suppressWarnings(as.numeric(x_chr))
  out <- as.Date(rep(NA_real_, length(x_chr)), origin = "1899-12-30")
  out[!is.na(serial)] <- as.Date(serial[!is.na(serial)], origin = "1899-12-30")

  faltantes <- is.na(out)
  out[faltantes] <- suppressWarnings(as.Date(x_chr[faltantes], format = "%d/%m/%Y"))

  faltantes <- is.na(out)
  out[faltantes] <- suppressWarnings(as.Date(x_chr[faltantes], format = "%Y-%m-%d"))

  out
}

fmt_dinheiro <- function(x) {
  paste0("R$ ", number(x, big.mark = ".", decimal.mark = ",", accuracy = 1))
}

fmt_dinheiro_curto <- function(x) {
  case_when(
    abs(x) >= 1e9 ~ paste0("R$ ", number(x / 1e9, decimal.mark = ",", accuracy = 0.1), " bi"),
    abs(x) >= 1e6 ~ paste0("R$ ", number(x / 1e6, decimal.mark = ",", accuracy = 0.1), " mi"),
    abs(x) >= 1e3 ~ paste0("R$ ", number(x / 1e3, decimal.mark = ",", accuracy = 0.1), " mil"),
    TRUE ~ fmt_dinheiro(x)
  )
}

fmt_pct <- function(x) {
  paste0(number(x, big.mark = ".", decimal.mark = ",", accuracy = 0.01), "%")
}

fmt_int <- function(x) {
  number(x, big.mark = ".", decimal.mark = ",", accuracy = 1)
}

fmt_milhoes_eixo <- function(x) {
  paste0("R$ ", number(x / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 0.1), " mi")
}

normalizar_porte <- function(x) {
  valor <- normalizar_texto(x)
  recode(
    valor,
    "MICRO" = "Micro",
    "PEQUENA" = "Pequena",
    "MEDIA" = "Média",
    "GRANDE" = "Grande",
    .default = if_else(is.na(x) | as.character(x) == "", "Não informado", str_to_sentence(str_to_lower(as.character(x))))
  )
}

normalizar_setor_cnae <- function(x) {
  valor <- normalizar_texto(x)
  recode(
    valor,
    "COMERCIO E SERVICOS" = "Comércio e serviços",
    "INDUSTRIA DE TRANSFORMACAO" = "Indústria de transformação",
    .default = if_else(is.na(x) | as.character(x) == "", "Não informado", str_to_sentence(str_to_lower(as.character(x))))
  )
}

normalizar_subsetor_cnae <- function(x) {
  valor <- normalizar_texto(x)
  recode(
    valor,
    "TELECOMUNICACOES" = "Telecomunicações",
    "EQUIP INFO ELETRONICO OTICO" = "Equip. info, eletrônico e ótico",
    "CONSTRUCAO" = "Construção",
    "ATIV IMOBIL PROFISSIONAL E ADM" = "Ativ. imobil., profissional e adm.",
    "OUTRAS ATIV SERVICOS" = "Outras ativ. serviços",
    .default = if_else(is.na(x) | as.character(x) == "", "Não informado", str_to_sentence(str_to_lower(as.character(x))))
  )
}

classificar_origem <- function(origem_aba, forma_de_apoio) {
  forma <- normalizar_texto(forma_de_apoio)
  case_when(
    origem_aba == "Indiretas Automáticas" ~ "Indireta automática",
    str_detect(forma, "INDIRETA") ~ "Indireta não automática",
    TRUE ~ "Direta"
  )
}

ler_base <- function() {
  base <- read_excel(arquivo_base, sheet = 1)

  if (!"origem_operacao" %in% names(base)) {
    base$origem_operacao <- classificar_origem(base$origem_aba, base$forma_de_apoio)
  }

  if (!"tipo_operacao" %in% names(base)) {
    base$tipo_operacao <- if_else(
      str_detect(base$origem_operacao, "^Indireta"),
      "Indireta",
      "Direta"
    )
  }

  base |>
    mutate(
      data = data_excel(data_da_contratacao),
      ano = as.integer(format(data, "%Y")),
      valor_operacao = numero_br(valor_operacao_r),
      valor_contratado = valor_operacao,
      valor_desembolsado = numero_br(valor_desembolsado_r),
      juros_numero = numero_br(juros),
      uf_original = as.character(uf),
      uf = if_else(str_to_upper(uf_original) %in% ufs_brasil, str_to_upper(uf_original), NA_character_),
      porte_do_cliente = factor(
        normalizar_porte(porte_do_cliente),
        levels = c("Micro", "Pequena", "Média", "Grande", "Não informado"),
        ordered = TRUE
      ),
      setor_cnae = normalizar_setor_cnae(setor_cnae),
      subsetor_cnae = normalizar_subsetor_cnae(subsetor_cnae_agrupado),
      origem_detalhada = as.character(origem_operacao),
      origem = factor(as.character(tipo_operacao), levels = c("Direta", "Indireta")),
      inovacao_limpa = case_when(
        normalizar_texto(inovacao) == "SIM" ~ "Sim",
        normalizar_texto(inovacao) == "NAO" ~ "Não",
        TRUE ~ as.character(inovacao)
      ),
      juros_faixa = case_when(
        is.na(juros_numero) ~ "Não informado",
        juros_numero < 3 ~ "Até 3%",
        juros_numero < 7 ~ "3% a 7%",
        juros_numero < 10 ~ "7% a 10%",
        TRUE ~ "Acima de 10%"
      )
    )
}

base_2024_2025 <- function() {
  ler_base() |>
    filter(ano %in% c(2024L, 2025L))
}

normalizar_numero_contrato <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    na_if("") |>
    na_if("NA")
}

contar_contratos_unicos <- function(df, ..., name = "contratos") {
  out <- df |>
    mutate(
      .contrato_id = normalizar_numero_contrato(numero_do_contrato),
      .indireta = normalizar_texto(origem) == "INDIRETA"
    ) |>
    group_by(...) |>
    summarise(
      .contratos_unicos = sum(.indireta, na.rm = TRUE) + n_distinct(.contrato_id[!.indireta], na.rm = TRUE),
      .groups = "drop"
    )

  names(out)[names(out) == ".contratos_unicos"] <- name
  out
}

resumir <- function(df, ...) {
  df |>
    group_by(...) |>
    summarise(
      contratos = n(),
      valor_contratado = sum(valor_contratado, na.rm = TRUE),
      valor_desembolsado = sum(valor_desembolsado, na.rm = TRUE),
      ticket_medio = valor_contratado / contratos,
      juros_medio = mean(juros_numero, na.rm = TRUE),
      peso_juros = sum(if_else(!is.na(juros_numero), valor_contratado, 0), na.rm = TRUE),
      juros_medio_ponderado = if_else(
        peso_juros > 0,
        sum(juros_numero * valor_contratado, na.rm = TRUE) / peso_juros,
        NA_real_
      ),
      .groups = "drop"
    ) |>
    select(-peso_juros)
}

formatar_tabela <- function(df) {
  df |>
    mutate(
      across(any_of(c("valor_contratado", "ticket_medio")), fmt_dinheiro),
      across(any_of(c("valor_desembolsado")), fmt_dinheiro),
      across(any_of(c("juros_medio", "juros_medio_ponderado")), fmt_pct),
      across(any_of(c("contratos", "contratos_2024", "contratos_2025", "delta_contratos")), fmt_int)
    )
}

comparar_2024_2025 <- function(df, dimensao) {
  df |>
    filter(ano %in% c(2024L, 2025L)) |>
    group_by(across(all_of(dimensao)), ano) |>
    summarise(
      contratos = n(),
      valor = sum(valor_contratado, na.rm = TRUE),
      juros = mean(juros_numero, na.rm = TRUE),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = ano,
      values_from = c(contratos, valor, juros),
      values_fill = list(contratos = 0, valor = 0)
    ) |>
    mutate(
      contratos_2024 = coalesce(contratos_2024, 0),
      contratos_2025 = coalesce(contratos_2025, 0),
      valor_2024 = coalesce(valor_2024, 0),
      valor_2025 = coalesce(valor_2025, 0),
      delta_contratos = contratos_2025 - contratos_2024,
      delta_valor = valor_2025 - valor_2024
    ) |>
    select(
      all_of(dimensao),
      contratos_2024,
      contratos_2025,
      delta_contratos,
      valor_2024,
      valor_2025,
      delta_valor,
      juros_2024,
      juros_2025
    )
}

tema_fust <- function() {
  theme_minimal(base_family = "Arial", base_size = 12) +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    )
}

tema_fust_escuro <- function() {
  theme_minimal(base_family = "Arial", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = cor_texto, face = "bold"),
      axis.title = element_blank(),
      axis.text = element_text(color = cor_texto, face = "bold"),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = cor_grade, linewidth = 0.75),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(color = cor_texto, face = "bold"),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.margin = margin(18, 16, 18, 16)
    )
}

tema_donut_escuro <- function() {
  theme_void(base_family = "Arial", base_size = 12) +
    theme(
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(color = cor_texto, face = "bold"),
      strip.background = element_rect(fill = "transparent", color = NA),
      strip.text = element_text(color = cor_texto, face = "bold", size = 12),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.margin = margin(18, 16, 18, 16)
    )
}


rotulos_segmento <- function(df, valor_col, label_col = "label") {
  valor <- df[[valor_col]]
  df[[label_col]] <- if_else(valor > 0, fmt_dinheiro_curto(valor), "")
  df
}

salvar_figura <- function(plot, nome, width = 9, height = 5) {
  ggsave(
    filename = file.path(output_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "transparent"
  )
  salvar_copia_secao(nome)
}

salvar_figura_escura <- function(plot, nome, width = 9, height = 5) {
  ggsave(
    filename = file.path(output_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "transparent"
  )
  salvar_copia_secao(nome)
}

salvar_tabela <- function(df, nome) {
  nomes_pretty <- c(
    ano = "Ano",
    contratos = "Contratos",
    contratos_2024 = "Contratos 2024",
    contratos_2025 = "Contratos 2025",
    delta_contratos = "Delta contratos",
    valor = "Valor",
    valor_2024 = "Valor 2024",
    valor_2025 = "Valor 2025",
    delta_valor = "Delta valor",
    valor_contratado = "Valor contratado",
    valor_contratado_r_milhoes = "Valor contratado (R$ milhões)",
    valor_desembolsado = "Valor desembolsado",
    valor_desembolsado_r_milhoes = "Valor desembolsado (R$ milhões)",
    valor_operacao = "Valor da operação",
    diferenca_operacao_desembolso = "Diferença entre operação e desembolso",
    percentual_desembolsado = "Percentual desembolsado",
    ticket_medio = "Ticket médio",
    ticket_medio_r_milhoes = "Ticket médio (R$ milhões)",
    juros = "Juros",
    juros_2024 = "Juros 2024",
    juros_2025 = "Juros 2025",
    juros_medio = "Juros médio",
    juros_medio_ponderado = "Juros médio ponderado",
    juros_faixa = "Faixa de juros",
    origem_detalhada = "Origem detalhada",
    origem = "Origem",
    uf = "UF",
    setor_cnae = "Setor CNAE",
    subsetor_cnae = "Subsetor CNAE",
    porte_do_cliente = "Porte do cliente",
    inovacao_limpa = "Inovação",
    cliente = "Cliente"
  )

  nomes <- names(df)
  nomes_formatados <- if_else(
    nomes %in% names(nomes_pretty),
    nomes_pretty[nomes],
    str_to_sentence(str_replace_all(nomes, "_", " "))
  )
  names(df) <- nomes_formatados

  write_excel_csv2(df, file.path(output_dir, nome), na = "")
  salvar_copia_secao(nome)
}
