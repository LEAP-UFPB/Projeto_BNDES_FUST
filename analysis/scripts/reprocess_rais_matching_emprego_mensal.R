#!/usr/bin/env Rscript

packages <- c("data.table", "readxl", "stringr", "writexl")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)

arg_value <- function(name, default = NULL) {
  prefix <- paste0("--", name, "=")
  inline <- args[str_starts(args, fixed(prefix))]
  if (length(inline) > 0) {
    return(str_sub(inline[1], str_length(prefix) + 1))
  }

  flag <- paste0("--", name)
  pos <- which(args == flag)
  if (length(pos) > 0 && length(args) >= pos[1] + 1) {
    return(args[pos[1] + 1])
  }

  default
}

parse_years <- function(value) {
  value <- str_replace_all(value, "\\s+", "")
  if (str_detect(value, "^\\d{4}:\\d{4}$")) {
    parts <- as.integer(str_split(value, ":", simplify = TRUE))
    return(seq(parts[1], parts[2]))
  }
  as.integer(str_split(value, ",", simplify = FALSE)[[1]])
}

repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(repo_root, "data")
rais_dir <- arg_value("rais", "D:/rais")
matching_file <- arg_value(
  "matching",
  file.path(data_dir, "RAIS_matching_tratadas_controles_2018_2024.xlsx")
)
if (!file.exists(matching_file)) {
  matching_file <- file.path(data_dir, "RAIS_matching_tratadas_controles_2018_2024.csv")
}
out_file <- arg_value(
  "out",
  file.path(data_dir, "RAIS_matching_emprego_mensal_2022_2024.xlsx")
)
years <- parse_years(arg_value("anos", "2022:2024"))

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

parse_admission_year <- function(x) {
  x <- clean_digits(x)
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x) & str_length(x) == 8
  out[ok] <- suppressWarnings(as.integer(str_sub(x[ok], 5, 8)))
  out
}

parse_admission_month <- function(x) {
  x <- clean_digits(x)
  out <- rep(NA_integer_, length(x))
  ok <- !is.na(x) & str_length(x) == 8
  out[ok] <- suppressWarnings(as.integer(str_sub(x[ok], 3, 4)))
  out[is.na(out) | out < 1 | out > 12] <- NA_integer_
  out
}

pick_col <- function(cols, patterns, required = TRUE) {
  normalized <- normalize_text(cols)
  for (pattern in patterns) {
    hit <- which(str_detect(normalized, pattern))
    if (length(hit) > 0) {
      return(cols[hit[1]])
    }
  }
  if (required) {
    stop(
      "Coluna nao encontrada. Padroes: ",
      paste(patterns, collapse = " | "),
      "\nColunas disponiveis: ",
      paste(cols, collapse = ", "),
      call. = FALSE
    )
  }
  NA_character_
}

detect_vinc_columns <- function(file) {
  header <- fread(
    file,
    nrows = 0,
    encoding = "Latin-1",
    showProgress = FALSE
  )
  cols <- names(header)
  list(
    cnpj = pick_col(cols, c("^CNPJ CEI$", "^CNPJ / CEI$")),
    cnpj_raiz = pick_col(cols, c("^CNPJ RAIZ$")),
    data_admissao = pick_col(cols, c("^DATA ADMISSAO", "^DATA ADMISSAO DECLARADA")),
    vinculo_ativo = pick_col(cols, c("VINCULO ATIVO 31 12")),
    mes_desligamento = pick_col(cols, c("^MES DESLIGAMENTO")),
    motivo_desligamento = pick_col(cols, c("^MOTIVO DESLIGAMENTO"), required = FALSE)
  )
}

read_selected <- function(file, columns) {
  selected <- unique(unlist(columns[!is.na(columns)]))
  dt <- fread(
    file,
    select = selected,
    colClasses = "character",
    integer64 = "character",
    encoding = "Latin-1",
    showProgress = TRUE
  )

  for (canonical in names(columns)) {
    original <- columns[[canonical]]
    if (!is.na(original) && original %in% names(dt)) {
      setnames(dt, original, canonical)
    }
  }

  dt
}

year_dir <- function(year) {
  candidates <- c(file.path(rais_dir, paste0("RAIS_", year)))
  candidates[file.exists(candidates)][1]
}

vinc_files_for_year <- function(year) {
  dir <- year_dir(year)
  if (is.na(dir)) {
    return(character())
  }
  list.files(
    dir,
    full.names = TRUE,
    recursive = FALSE,
    pattern = "(?i)^RAIS_VINC.*\\.txt$"
  )
}

message("Lendo painel pos-matching: ", matching_file)
if (str_detect(tolower(matching_file), "\\.xlsx$")) {
  matching <- as.data.table(read_excel(matching_file, sheet = 1, col_types = "text"))
} else {
  matching <- fread(matching_file, sep = ";", colClasses = "character", encoding = "UTF-8")
}
matching[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
roots_matching <- sort(unique(matching[!is.na(cnpj_raiz), cnpj_raiz]))

painel_fust_file <- file.path(data_dir, "Painel_CNPJ_FUST_RAIS_2018_2024.xlsx")
roots_fust <- character()
if (file.exists(painel_fust_file)) {
  painel_fust <- as.data.table(read_excel(
    painel_fust_file,
    sheet = "painel_cnpj_ano",
    col_types = "text"
  ))
  painel_fust[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
  roots_fust <- sort(unique(painel_fust[!is.na(cnpj_raiz), cnpj_raiz]))
}

roots <- sort(unique(c(roots_matching, roots_fust)))

if (length(roots) == 0) {
  stop("Painel pos-matching sem CNPJ raiz valido.", call. = FALSE)
}

message("CNPJs raiz no painel pos-matching: ", length(roots_matching))
message("CNPJs raiz de apoiadas FUST no painel RAIS: ", length(roots_fust))
message("CNPJs raiz processados no total: ", length(roots))
monthly_parts <- list()
monthly_cnpj_parts <- list()

for (year in years) {
  files <- vinc_files_for_year(year)
  if (length(files) == 0) {
    warning("Sem arquivos RAIS_VINC para ", year)
    next
  }

  message("Processando RAIS vinculos ", year, ": ", length(files), " arquivo(s)")
  year_parts <- list()
  for (file in files) {
    message("Arquivo: ", basename(file))
    cols <- detect_vinc_columns(file)
    dt <- read_selected(file, cols)
    dt[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
    dt[, cnpj_completo := str_pad(clean_digits(cnpj), 14, pad = "0")]
    dt <- dt[cnpj_raiz %in% roots]
    if (nrow(dt) == 0) {
      rm(dt)
      gc()
      next
    }

    dt[, ano := year]
    dt[, adm_ano := parse_admission_year(data_admissao)]
    dt[, adm_mes := parse_admission_month(data_admissao)]
    dt[, ativo_31_12 := parse_number_br(vinculo_ativo) == 1]
    dt[, mes_desl := as.integer(parse_number_br(mes_desligamento))]
    dt[is.na(mes_desl) | mes_desl < 1 | mes_desl > 12, mes_desl := NA_integer_]

    dt[, mes_inicio := fifelse(!is.na(adm_ano) & adm_ano == year, adm_mes, 1L)]
    dt[is.na(mes_inicio) | mes_inicio < 1, mes_inicio := 1L]
    dt[adm_ano > year, mes_inicio := 13L]
    dt[, mes_fim := fifelse(!is.na(mes_desl) & !ativo_31_12, mes_desl, 12L)]
    dt[is.na(mes_fim) | mes_fim > 12, mes_fim := 12L]
    dt <- dt[mes_inicio <= 12 & mes_fim >= 1 & mes_inicio <= mes_fim]
    if (nrow(dt) == 0) {
      rm(dt)
      gc()
      next
    }

    for (month in 1:12) {
      col <- paste0("empregados_m", str_pad(month, 2, pad = "0"))
      dt[, (col) := as.integer(mes_inicio <= month & mes_fim >= month)]
    }
    month_cols <- paste0("empregados_m", str_pad(1:12, 2, pad = "0"))
    agg <- dt[, lapply(.SD, sum, na.rm = TRUE), by = .(cnpj_raiz, ano), .SDcols = month_cols]
    year_parts[[length(year_parts) + 1]] <- agg
    agg_cnpj <- dt[
      !is.na(cnpj_completo) & cnpj_completo != "00000000000000",
      lapply(.SD, sum, na.rm = TRUE),
      by = .(cnpj_raiz, cnpj_completo, ano),
      .SDcols = month_cols
    ]
    monthly_cnpj_parts[[length(monthly_cnpj_parts) + 1]] <- agg_cnpj
    rm(dt, agg, agg_cnpj)
    gc()
  }

  if (length(year_parts) > 0) {
    year_dt <- rbindlist(year_parts, use.names = TRUE, fill = TRUE)
    month_cols <- paste0("empregados_m", str_pad(1:12, 2, pad = "0"))
    year_dt <- year_dt[, lapply(.SD, sum, na.rm = TRUE), by = .(cnpj_raiz, ano), .SDcols = month_cols]
    monthly_parts[[length(monthly_parts) + 1]] <- year_dt
  }
}

if (length(monthly_parts) == 0) {
  stop("Nenhum vinculo encontrado para os CNPJs do painel pos-matching.", call. = FALSE)
}

monthly <- rbindlist(monthly_parts, use.names = TRUE, fill = TRUE)
month_cols <- paste0("empregados_m", str_pad(1:12, 2, pad = "0"))
monthly <- monthly[, lapply(.SD, sum, na.rm = TRUE), by = .(cnpj_raiz, ano), .SDcols = month_cols]
monthly[, estoque_medio_anual_empregados := rowMeans(.SD, na.rm = TRUE), .SDcols = month_cols]
setorder(monthly, cnpj_raiz, ano)

monthly_cnpj <- rbindlist(monthly_cnpj_parts, use.names = TRUE, fill = TRUE)
monthly_cnpj <- monthly_cnpj[
  ,
  lapply(.SD, sum, na.rm = TRUE),
  by = .(cnpj_raiz, cnpj_completo, ano),
  .SDcols = month_cols
]
monthly_cnpj[, estoque_medio_anual_empregados := rowMeans(.SD, na.rm = TRUE), .SDcols = month_cols]
setorder(monthly_cnpj, cnpj_raiz, cnpj_completo, ano)

coverage <- data.table(
  cnpjs_pos_matching = length(roots_matching),
  cnpjs_apoiadas_fust = length(roots_fust),
  cnpjs_processados = length(roots),
  cnpjs_com_vinculos = uniqueN(monthly$cnpj_raiz),
  cnpjs_completos_com_vinculos = uniqueN(monthly_cnpj$cnpj_completo),
  anos = paste(sort(unique(monthly$ano)), collapse = ", "),
  linhas = nrow(monthly),
  linhas_cnpj_completo = nrow(monthly_cnpj),
  metodo = "Estoque medio anual calculado como media dos 12 estoques mensais reconstruidos dos microdados RAIS_VINC."
)

message("Gravando: ", out_file)
write_xlsx(
  list(
    emprego_mensal = as.data.frame(monthly),
    emprego_mensal_cnpj = as.data.frame(monthly_cnpj),
    cobertura = as.data.frame(coverage)
  ),
  out_file
)
message("Concluido. Linhas: ", nrow(monthly))
