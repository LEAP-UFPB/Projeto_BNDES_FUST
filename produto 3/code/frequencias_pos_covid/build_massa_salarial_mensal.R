#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[1], fixed = TRUE) else default
}
all_args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", all_args[grepl("^--file=", all_args)][1]),
                             winslash = "/", mustWork = TRUE)
produto_dir <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(produto_dir, ".."), winslash = "/", mustWork = TRUE)
rais_dir <- arg_value("rais", "D:/rais")
matched_file <- arg_value("matched", file.path(produto_dir, "analysis", "psm_did", "unidades_pareadas.csv"))
out_file <- arg_value("out", file.path(produto_dir, "analysis", "frequencias_pos_covid", "massa_salarial_mensal_2022_2024.csv"))
years <- as.integer(strsplit(arg_value("anos", "2022,2023,2024"), ",", fixed = TRUE)[[1]])
dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)

normalize_text <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  str_squish(str_replace_all(x, "[^A-Z0-9]+", " "))
}
clean_root <- function(x) {
  x <- str_replace_all(as.character(x), "\\D", "")
  x <- str_pad(x, 8, side = "left", pad = "0")
  fifelse(nchar(x) > 8, substr(x, 1, 8), x)
}
parse_num <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  comma <- !is.na(x) & grepl(",", x, fixed = TRUE)
  x[comma] <- gsub(".", "", x[comma], fixed = TRUE)
  x[comma] <- gsub(",", ".", x[comma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
pick <- function(cols, pattern) {
  hit <- which(str_detect(normalize_text(cols), pattern))
  if (!length(hit)) NA_character_ else cols[hit[1]]
}

units <- fread(matched_file, sep = ";", colClasses = c(cnpj_raiz = "character"))
roots <- unique(clean_root(units$cnpj_raiz))
pattern_file <- tempfile(fileext = ".txt")
writeLines(c(paste0(";", roots, ";"), paste0(",\"", roots, "\","), paste0(",", roots, ",")),
           pattern_file, useBytes = TRUE)
on.exit(unlink(pattern_file), add = TRUE)
month_patterns <- c(
  "VL REM JANEIRO SC", "VL REM FEVEREIRO SC", "VL REM MARCO SC",
  "VL REM ABRIL SC", "VL REM MAIO SC", "VL REM JUNHO SC",
  "VL REM JULHO SC", "VL REM AGOSTO SC", "VL REM SETEMBRO SC",
  "VL REM OUTUBRO SC", "VL REM NOVEMBRO SC", "^VL REM(UN)? DEZEMBRO NOM"
)

parts <- list()
for (year in years) {
  files <- list.files(file.path(rais_dir, paste0("RAIS_", year)),
                      pattern = "(?i)^RAIS_VINC.*\\.(txt|comt)$", full.names = TRUE)
  message("Ano ", year, ": ", length(files), " arquivos")
  for (file in files) {
    cols <- names(fread(file, nrows = 0, encoding = "Latin-1", showProgress = FALSE))
    root_col <- pick(cols, "^CNPJ RAIZ")
    rem_cols <- unname(vapply(month_patterns, function(p) pick(cols, p), character(1)))
    if (is.na(root_col) || anyNA(rem_cols)) {
      warning("Colunas mensais incompletas em ", basename(file))
      next
    }
    filtered_file <- tempfile(fileext = ".txt")
    status <- system2("rg", c("--text", "-f", shQuote(pattern_file), shQuote(file)),
                      stdout = filtered_file, stderr = TRUE)
    if (!file.exists(filtered_file) || file.info(filtered_file)$size == 0) {
      unlink(filtered_file)
      next
    }
    selected_idx <- match(c(root_col, rem_cols), cols)
    dt <- fread(filtered_file, header = FALSE, select = selected_idx, encoding = "Latin-1",
                showProgress = FALSE)
    unlink(filtered_file)
    if (ncol(dt) != 13L) {
      stop("Número inesperado de colunas após filtro em ", basename(file), ": ", ncol(dt), call. = FALSE)
    }
    setnames(dt, names(dt), c("cnpj_raiz", paste0("m", sprintf("%02d", 1:12))))
    dt[, cnpj_raiz := clean_root(cnpj_raiz)]
    dt <- dt[cnpj_raiz %in% roots]
    if (!nrow(dt)) next
    for (v in paste0("m", sprintf("%02d", 1:12))) dt[, (v) := parse_num(get(v))]
    parts[[length(parts) + 1L]] <- dt[, lapply(.SD, sum, na.rm = TRUE),
                                      by = cnpj_raiz, .SDcols = patterns("^m\\d{2}$")][, ano := year]
  }
}
if (!length(parts)) stop("Nenhuma remuneração encontrada.", call. = FALSE)
out <- rbindlist(parts, fill = TRUE)
out <- out[, lapply(.SD, sum, na.rm = TRUE), by = .(cnpj_raiz, ano),
           .SDcols = patterns("^m\\d{2}$")]
long <- melt(out, id.vars = c("cnpj_raiz", "ano"), variable.name = "mes",
             value.name = "massa_salarial")
long[, mes := as.integer(sub("m", "", mes))]
setorder(long, cnpj_raiz, ano, mes)
fwrite(long, out_file, sep = ";", bom = TRUE, na = "")
message("Gravado: ", out_file, " (", nrow(long), " linhas)")
