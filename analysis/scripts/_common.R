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
      origem_detalhada = as.character(origem_operacao),
      origem = as.character(tipo_operacao),
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

salvar_figura <- function(plot, nome, width = 9, height = 5) {
  ggsave(
    filename = file.path(output_dir, nome),
    plot = plot,
    width = width,
    height = height,
    dpi = 220,
    bg = "white"
  )
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
    valor_desembolsado = "Valor desembolsado",
    valor_operacao = "Valor da operação",
    diferenca_operacao_desembolso = "Diferença entre operação e desembolso",
    percentual_desembolsado = "Percentual desembolsado",
    ticket_medio = "Ticket médio",
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
}
