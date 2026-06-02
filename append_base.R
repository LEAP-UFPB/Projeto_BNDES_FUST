#!/usr/bin/env Rscript

pacotes <- c(
  "dplyr",
  "janitor",
  "purrr",
  "readr",
  "readxl",
  "stringr",
  "tidyr",
  "writexl"
)

faltando <- setdiff(pacotes, rownames(installed.packages()))

if (length(faltando) > 0) {
  install.packages(faltando)
}

suppressPackageStartupMessages({
  invisible(lapply(pacotes, library, character.only = TRUE))
})

arquivo_entrada <- "C:/Github/BNDES FUST/data/Base de Operações_FUST (enviada).xlsx"
arquivo_saida   <- "C:/Github/BNDES FUST/data/Base de Operações_FUST_compilada.xlsx"

abas_entrada <- tibble(
  aba = c(1, 2),
  origem_aba = c("Dir. e Indir. Não-automática", "Indiretas Automáticas")
)

colunas_cnpj <- c(
  "cnpj",
  "cnpj_cpf",
  "cpf_cnpj",
  "cpf_ou_cnpj",
  "cnpj_ou_cpf",
  "cnpj_cliente",
  "cnpj_do_cliente",
  "cpf_cnpj_cliente",
  "cpf_cnpj_do_cliente",
  "cnpj_beneficiario",
  "cnpj_do_beneficiario",
  "cpf_cnpj_beneficiario",
  "cpf_cnpj_do_beneficiario",
  "cnpj_empresa",
  "cnpj_da_empresa",
  "cnpj_empresa_beneficiaria",
  "cnpj_da_empresa_beneficiaria"
)

colunas_nome <- c(
  "cliente",
  "beneficiario",
  "beneficiaria",
  "mutuario",
  "mutuaria",
  "tomador",
  "tomadora",
  "favorecido",
  "favorecida",
  "nome_do_cliente",
  "nome_do_beneficiario",
  "nome_do_mutuario",
  "nome_do_tomador",
  "razao_social",
  "razao_social_do_cliente",
  "empresa",
  "empresa_beneficiaria",
  "nome_da_empresa"
)

colunas_valor_operacao <- c(
  "valor_contratado_r",
  "valor_contratado_rs",
  "valor_contratado",
  "valor_da_operacao",
  "valor_da_operacao_r",
  "valor_da_operacao_em_r",
  "valor_financiado",
  "valor"
)

colunas_valor_desembolsado <- c(
  "valor_desembolsado_r",
  "valor_desembolsado_rs",
  "valor_desembolsado",
  "valor_do_desembolso",
  "valor_do_desembolso_r",
  "valor_do_desembolso_em_r"
)

normalizar_texto <- function(x) {
  x |>
    as.character() |>
    iconv(to = "ASCII//TRANSLIT") |>
    str_to_upper() |>
    str_replace_all("[^A-Z0-9]+", " ") |>
    str_squish() |>
    na_if("")
}

detectar_skip <- function(aba) {
  previa <- read_excel(
    arquivo_entrada,
    sheet = aba,
    col_names = FALSE,
    n_max = 80,
    col_types = "text"
  )

  previa <- as.data.frame(previa)

  score_linha <- apply(previa, 1, function(linha) {
    linha <- normalizar_texto(linha)

    sum(
      str_detect(
        linha,
        "CNPJ|CPF|VALOR|CONTRAT|OPERACAO|CLIENTE|BENEFICI|MUTUAR|TOMADOR|UF|MUNICIPIO"
      ),
      na.rm = TRUE
    )
  })

  primeira_linha_cabecalho <- dplyr::first(which(score_linha >= 2), default = 1)

  primeira_linha_cabecalho - 1
}

achar_colunas <- function(df, candidatos, padrao) {
  unique(c(
    intersect(candidatos, names(df)),
    names(df)[str_detect(names(df), padrao)]
  ))
}

coalesce_texto <- function(df, cols) {
  if (length(cols) == 0) {
    return(rep(NA_character_, nrow(df)))
  }

  cols |>
    map(function(col) na_if(str_squish(as.character(df[[col]])), "")) |>
    reduce(coalesce)
}

normalizar_valor <- function(x) {
  x <- x |>
    as.character() |>
    str_squish() |>
    str_replace_all("R\\$|\\s", "")

  x <- na_if(x, "")
  x <- na_if(x, "-")
  x <- na_if(x, "----------")

  x <- if_else(
    str_detect(x, ","),
    str_replace_all(str_replace_all(x, "\\.", ""), ",", "."),
    x
  )

  parse_number(x, locale = locale(decimal_mark = "."))
}

coalesce_valor <- function(df, cols) {
  if (length(cols) == 0) {
    return(rep(NA_real_, nrow(df)))
  }

  cols |>
    map(function(col) normalizar_valor(df[[col]])) |>
    reduce(coalesce)
}

limpar_cnpj <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- na_if(x, "")
  x <- if_else(str_detect(x, "\\*"), NA_character_, x)
  x <- str_replace_all(x, "\\D", "")
  x <- na_if(x, "")
  x <- str_pad(x, 14, pad = "0")
  x <- na_if(x, str_dup("0", 14))

  x
}

normalizar_cnpj_mascarado <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x <- na_if(x, "")
  x <- if_else(str_detect(x, "\\*"), x, NA_character_)

  x
}

formatar_cnpj <- function(x) {
  limpar_cnpj(x) |>
    str_replace(
      "^(\\d{2})(\\d{3})(\\d{3})(\\d{4})(\\d{2})$",
      "\\1.\\2.\\3/\\4-\\5"
    )
}

formatar_data_excel <- function(x) {
  x_chr <- as.character(x) |>
    str_squish() |>
    na_if("")

  serial <- suppressWarnings(as.numeric(x_chr))
  data <- as.Date(rep(NA_real_, length(x_chr)), origin = "1899-12-30")
  data[!is.na(serial)] <- as.Date(serial[!is.na(serial)], origin = "1899-12-30")

  faltantes <- is.na(data)
  data[faltantes] <- suppressWarnings(as.Date(x_chr[faltantes], format = "%d/%m/%Y"))

  faltantes <- is.na(data)
  data[faltantes] <- suppressWarnings(as.Date(x_chr[faltantes], format = "%Y-%m-%d"))

  if_else(is.na(data), x_chr, format(data, "%d/%m/%Y"))
}

normalizar_porte <- function(x) {
  valor <- normalizar_texto(x)
  recode(
    valor,
    "MICRO" = "Micro",
    "PEQUENA" = "Pequena",
    "MEDIA" = "Média",
    "GRANDE" = "Grande",
    .default = if_else(is.na(x) | as.character(x) == "", NA_character_, str_to_sentence(str_to_lower(as.character(x))))
  )
}

normalizar_setor_cnae <- function(x) {
  valor <- normalizar_texto(x)
  recode(
    valor,
    "COMERCIO E SERVICOS" = "Comércio e serviços",
    "INDUSTRIA DE TRANSFORMACAO" = "Indústria de transformação",
    .default = if_else(is.na(x) | as.character(x) == "", NA_character_, str_to_sentence(str_to_lower(as.character(x))))
  )
}

classificar_operacao <- function(origem_aba, forma_de_apoio) {
  forma <- normalizar_texto(forma_de_apoio)

  case_when(
    origem_aba == "Indiretas Automáticas" ~ "Indireta automática",
    str_detect(forma, "INDIRETA") ~ "Indireta não automática",
    TRUE ~ "Direta"
  )
}

ler_aba <- function(aba, origem_aba) {
  skip <- detectar_skip(aba)

  message("Lendo aba ", aba, " | skip = ", skip)

  read_excel(
    arquivo_entrada,
    sheet = aba,
    skip = skip,
    guess_max = 100000,
    col_types = "text"
  ) |>
    clean_names() |>
    mutate(origem_aba = origem_aba, .before = 1)
}

message("Lendo arquivo: ", arquivo_entrada)

base_bruta <- pmap_dfr(abas_entrada, ler_aba)

colunas_cnpj_base <- achar_colunas(
  base_bruta,
  colunas_cnpj,
  "cnpj|cpf"
)

colunas_valor_operacao_base <- achar_colunas(
  base_bruta,
  colunas_valor_operacao,
  "valor.*(contrat|operacao|financi)|operacao.*valor|contrat.*valor"
)

colunas_valor_desembolsado_base <- achar_colunas(
  base_bruta,
  colunas_valor_desembolsado,
  "valor.*desembols|desembols.*valor"
)

colunas_nome_base <- achar_colunas(
  base_bruta,
  colunas_nome,
  "cliente|benefici|mutuar|tomador|empresa|razao"
)

message("Colunas de CNPJ/CPF encontradas: ", paste(colunas_cnpj_base, collapse = ", "))
message("Colunas de valor de operação encontradas: ", paste(colunas_valor_operacao_base, collapse = ", "))
message("Colunas de valor desembolsado encontradas: ", paste(colunas_valor_desembolsado_base, collapse = ", "))
message("Colunas de nome encontradas: ", paste(colunas_nome_base, collapse = ", "))

if (length(colunas_cnpj_base) == 0) {
  stop(
    "Não achei coluna de CNPJ/CPF. Colunas lidas: ",
    paste(names(base_bruta), collapse = ", "),
    call. = FALSE
  )
}

if (length(colunas_valor_operacao_base) == 0) {
  stop(
    "Não achei coluna de valor contratado/operação. Colunas lidas: ",
    paste(names(base_bruta), collapse = ", "),
    call. = FALSE
  )
}

if (length(colunas_valor_desembolsado_base) == 0) {
  stop(
    "Não achei coluna de valor desembolsado. Colunas lidas: ",
    paste(names(base_bruta), collapse = ", "),
    call. = FALSE
  )
}

if (length(colunas_nome_base) == 0) {
  stop(
    "Não achei coluna de nome/cliente para preencher CNPJ. Colunas lidas: ",
    paste(names(base_bruta), collapse = ", "),
    call. = FALSE
  )
}

base_trabalho <- base_bruta

base_trabalho$cnpj_original <- coalesce_texto(base_trabalho, colunas_cnpj_base)

base_trabalho$cnpj_lido <- limpar_cnpj(base_trabalho$cnpj_original)

base_trabalho$cnpj_mascarado <- normalizar_cnpj_mascarado(base_trabalho$cnpj_original)

base_trabalho$chave_cnpj <- normalizar_texto(
  coalesce_texto(base_trabalho, colunas_nome_base)
)

base_trabalho$valor_operacao_r <- coalesce_valor(
  base_trabalho,
  colunas_valor_operacao_base
)

base_trabalho$valor_desembolsado_r <- coalesce_valor(
  base_trabalho,
  colunas_valor_desembolsado_base
)

mapa_cnpj <- base_trabalho |>
  filter(!is.na(cnpj_lido), !is.na(chave_cnpj)) |>
  distinct(chave_cnpj, cnpj_lido) |>
  group_by(chave_cnpj) |>
  summarise(
    cnpj_preenchido = if_else(
      n_distinct(cnpj_lido) == 1,
      first(cnpj_lido),
      NA_character_
    ),
    .groups = "drop"
  ) |>
  filter(!is.na(cnpj_preenchido))

colunas_remover <- setdiff(
  c(
    colunas_cnpj_base,
    colunas_valor_operacao_base,
    colunas_valor_desembolsado_base,
    "cnpj_original",
    "cnpj_lido",
    "cnpj_mascarado",
    "chave_cnpj",
    "cnpj_preenchido"
  ),
  c("cnpj", "valor_operacao_r", "valor_desembolsado_r")
)

base_compilada <- base_trabalho |>
  left_join(mapa_cnpj, by = "chave_cnpj") |>
  mutate(
    cnpj = coalesce(
      cnpj_mascarado,
      formatar_cnpj(coalesce(cnpj_lido, cnpj_preenchido))
    ),
    origem_operacao = classificar_operacao(origem_aba, forma_de_apoio),
    tipo_operacao = if_else(
      str_detect(origem_operacao, "^Indireta"),
      "Indireta",
      "Direta"
    ),
    data_da_contratacao = formatar_data_excel(data_da_contratacao),
    uf = str_to_upper(uf),
    porte_do_cliente = normalizar_porte(porte_do_cliente),
    setor_cnae = normalizar_setor_cnae(setor_cnae)
  ) |>
  select(
    tipo_operacao,
    origem_operacao,
    origem_aba,
    cnpj,
    valor_operacao_r,
    valor_desembolsado_r,
    everything(),
    -any_of(colunas_remover)
  )

writexl::write_xlsx(
  list(base_compilada = base_compilada),
  path = arquivo_saida
)

message("Arquivo gerado: ", arquivo_saida)
message("Total de linhas compiladas: ", nrow(base_compilada))
