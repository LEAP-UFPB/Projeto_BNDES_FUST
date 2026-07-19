#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("analysis/scripts/reprocess_rais_universo_empresas.R", winslash = "/", mustWork = TRUE)
}
repo_root <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
rais_dir <- "D:/rais"
years <- 2018:2024
out_file <- file.path(repo_root, "data", "RAIS_universo_empresas_2018_2024.csv")

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  str_squish(x)
}

clean_digits <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\D", "")
  x[x == ""] <- NA_character_
  x
}

parse_number_br <- function(x) {
  x <- as.character(x)
  x <- str_squish(x)
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  has_comma <- !is.na(x) & str_detect(x, ",")
  x[has_comma] <- str_replace_all(x[has_comma], "\\.", "")
  x[has_comma] <- str_replace_all(x[has_comma], ",", ".")
  suppressWarnings(as.numeric(x))
}

uf_from_municipio <- function(municipio) {
  cod <- str_pad(clean_digits(municipio), 6, pad = "0")
  uf_code <- str_sub(cod, 1, 2)
  uf_map <- c(
    "11" = "RO", "12" = "AC", "13" = "AM", "14" = "RR", "15" = "PA",
    "16" = "AP", "17" = "TO", "21" = "MA", "22" = "PI", "23" = "CE",
    "24" = "RN", "25" = "PB", "26" = "PE", "27" = "AL", "28" = "SE",
    "29" = "BA", "31" = "MG", "32" = "ES", "33" = "RJ", "35" = "SP",
    "41" = "PR", "42" = "SC", "43" = "RS", "50" = "MS", "51" = "MT",
    "52" = "GO", "53" = "DF"
  )
  unname(uf_map[uf_code])
}

faixa_emprego <- function(x) {
  fifelse(
    is.na(x), "Sem informacao",
    fifelse(
      x <= 0, "0",
      fifelse(
        x <= 9, "1 a 9",
        fifelse(
          x <= 49, "10 a 49",
          fifelse(
            x <= 99, "50 a 99",
            fifelse(x <= 249, "100 a 249", fifelse(x <= 499, "250 a 499", "500 ou mais"))
          )
        )
      )
    )
  )
}

pick_col <- function(cols, patterns) {
  normalized <- normalize_text(cols)
  for (pattern in patterns) {
    hit <- which(str_detect(normalized, pattern))
    if (length(hit) > 0) {
      return(cols[hit[1]])
    }
  }
  stop("Coluna nao encontrada: ", paste(patterns, collapse = " | "))
}

estab_file_for_year <- function(year) {
  dir <- file.path(rais_dir, paste0("RAIS_", year))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  files <- files[
    str_detect(basename(files), regex("estb|estab", ignore_case = TRUE)) &
      str_detect(basename(files), regex("\\.(txt|comt)$", ignore_case = TRUE))
  ]
  if (length(files) == 0) {
    stop("Sem arquivo de estabelecimentos para ", year, call. = FALSE)
  }
  files <- files[order(
    !str_detect(basename(files), regex("COMT$", ignore_case = TRUE)),
    !str_detect(basename(files), regex("RAIS_ESTAB", ignore_case = TRUE)),
    basename(files)
  )]
  files[1]
}

detect_cols <- function(file) {
  header <- fread(file, nrows = 0, encoding = "Latin-1", showProgress = FALSE)
  cols <- names(header)
  list(
    cnpj_raiz = pick_col(cols, c("^CNPJ RAIZ")),
    municipio = pick_col(cols, c("^MUNICIPIO")),
    empregados = pick_col(cols, c("^QTD VINCULOS ATIVOS")),
    cnae_classe = pick_col(cols, c("^CNAE 2 0 CLASSE", "^CNAE 95 CLASSE")),
    cnae_subclasse = pick_col(cols, c("^CNAE 2 0 SUBCLASSE")),
    subsetor_ibge = pick_col(cols, c("^IBGE SUBSETOR"))
  )
}

process_year <- function(year) {
  file <- estab_file_for_year(year)
  message("Processando RAIS estabelecimentos ", year, ": ", file)
  cols <- detect_cols(file)
  dt <- fread(
    file,
    select = unique(unlist(cols)),
    colClasses = "character",
    integer64 = "character",
    encoding = "Latin-1",
    showProgress = TRUE
  )
  for (canonical in names(cols)) {
    setnames(dt, cols[[canonical]], canonical)
  }
  dt[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
  dt <- dt[!is.na(cnpj_raiz) & cnpj_raiz != "00000000"]
  dt[, ano := year]
  dt[, total_empregados_linha := parse_number_br(empregados)]
  dt[, uf := uf_from_municipio(municipio)]
  dt[, cnae_classe := clean_digits(cnae_classe)]
  dt[, cnae_subclasse := clean_digits(cnae_subclasse)]
  dt[, subsetor_ibge := str_pad(clean_digits(subsetor_ibge), 2, pad = "0")]

  base <- dt[
    ,
    .(
      total_empregados = sum(total_empregados_linha, na.rm = TRUE),
      total_firmas = .N
    ),
    by = .(ano, cnpj_raiz)
  ]

  uf <- dt[
    ,
    .(empregados_uf = sum(total_empregados_linha, na.rm = TRUE)),
    by = .(ano, cnpj_raiz, uf)
  ][order(ano, cnpj_raiz, -empregados_uf)]
  uf <- uf[, .SD[1], by = .(ano, cnpj_raiz)]

  setor <- dt[
    ,
    .(empregados_setor_principal = sum(total_empregados_linha, na.rm = TRUE)),
    by = .(ano, cnpj_raiz, cnae_classe, cnae_subclasse, subsetor_ibge)
  ][order(ano, cnpj_raiz, -empregados_setor_principal)]
  setor <- setor[, .SD[1], by = .(ano, cnpj_raiz)]

  out <- merge(base, uf[, .(ano, cnpj_raiz, uf_principal = uf)], by = c("ano", "cnpj_raiz"), all.x = TRUE)
  out <- merge(
    out,
    setor[, .(
      ano,
      cnpj_raiz,
      setor_cnae_classe_principal = cnae_classe,
      setor_cnae_subclasse_principal = cnae_subclasse,
      subsetor_ibge_principal_codigo = subsetor_ibge,
      empregados_setor_principal
    )],
    by = c("ano", "cnpj_raiz"),
    all.x = TRUE
  )
  message("Ano ", year, ": empresas=", nrow(out), " empregados=", round(sum(out$total_empregados, na.rm = TRUE)))
  out
}

painel <- rbindlist(lapply(years, process_year), fill = TRUE)
setorder(painel, cnpj_raiz, ano)
painel[, empregados_lag := shift(total_empregados), by = cnpj_raiz]
painel[, ano_lag := shift(ano), by = cnpj_raiz]
painel[, variacao_pct_empregados := (total_empregados - empregados_lag) / empregados_lag]
painel[is.na(empregados_lag) | empregados_lag <= 0 | ano_lag != ano - 1, variacao_pct_empregados := NA_real_]
painel[, faixa_emprego_pre := faixa_emprego(empregados_lag)]

fwrite(painel, out_file, sep = ";", bom = TRUE, na = "")
message("Arquivo salvo: ", out_file)
message("Linhas: ", nrow(painel), " | empresas: ", uniqueN(painel$cnpj_raiz))
