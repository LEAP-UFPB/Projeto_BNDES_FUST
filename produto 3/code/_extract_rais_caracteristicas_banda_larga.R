#!/usr/bin/env Rscript

# Extrai dos microdados de vinculos da RAIS as caracteristicas pre-FUST de
# todas as prestadoras observadas na Anatel. O filtro por CNPJ e feito com rg
# antes da leitura em R para evitar carregar dezenas de GB na memoria.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(FALSE)
script <- normalizePath(
  sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/"
)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/")
data_dir <- file.path(repo, "data")
rais_dir <- Sys.getenv("RAIS_DIR", unset = "D:/rais")
output_file <- file.path(
  data_dir, "produto3_caracteristicas_rais_banda_larga.csv"
)

clean_root <- function(x) {
  sprintf("%08d", suppressWarnings(as.integer(gsub("\\D", "", as.character(x)))))
}

num_br <- function(x) {
  z <- as.character(x)
  comma <- grepl(",", z, fixed = TRUE)
  z[comma] <- gsub(",", ".", gsub(".", "", z[comma], fixed = TRUE), fixed = TRUE)
  suppressWarnings(as.numeric(z))
}

mode_value <- function(x) {
  z <- as.character(x)
  z <- z[!is.na(z) & nzchar(z)]
  if (!length(z)) return(NA_character_)
  names(which.max(table(z)))
}

panel <- fread(
  file.path(data_dir, "produto3_painel_banda_larga_fixa.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
roots <- unique(clean_root(panel[observada_no_ano == TRUE, cnpj_raiz]))
roots <- roots[grepl("^[0-9]{8}$", roots)]
rg <- Sys.which("rg")
if (!nzchar(rg)) stop("ripgrep (rg) nao encontrado.")

annual <- list()
for (yr in 2023:2024) {
  checkpoint <- file.path(
    data_dir, paste0("produto3_caracteristicas_rais_banda_larga_", yr, ".csv")
  )
  if (file.exists(checkpoint)) {
    annual[[as.character(yr)]] <- fread(
      checkpoint, sep = ";", colClasses = c(cnpj_raiz = "character")
    )
    message("Checkpoint carregado: ", basename(checkpoint))
    next
  }

  pattern_file <- tempfile(fileext = ".txt")
  hits_file <- tempfile(fileext = ".txt")
  writeLines(paste0(",\"", roots, "\","), pattern_file, useBytes = TRUE)
  folder <- file.path(rais_dir, paste0("RAIS_", yr))
  message("Filtrando vinculos RAIS ", yr, " para ", length(roots), " CNPJs...")
  status <- system2(
    rg,
    c(
      "--no-line-number", "--no-filename", "--text", "--fixed-strings",
      "--file", shQuote(pattern_file), "--glob", shQuote("*VINC*"),
      shQuote(folder)
    ),
    stdout = hits_file, stderr = FALSE
  )
  unlink(pattern_file)
  if (status > 1L) stop("Falha ao filtrar RAIS ", yr, " (rg status ", status, ").")
  if (!file.exists(hits_file) || file.info(hits_file)$size == 0) {
    stop("Nenhum vinculo encontrado na RAIS ", yr, ".")
  }

  d <- fread(
    hits_file, header = FALSE,
    select = c(1L, 3L, 11L, 12L, 14L, 16L, 23L, 27L, 37L, 40L, 41L, 59L),
    colClasses = "character", encoding = "Latin-1", showProgress = FALSE
  )
  unlink(hits_file)
  setnames(d, c(
    "municipio", "ativo", "escolaridade", "sexo", "raca", "tamanho",
    "salario_medio", "tempo_emprego", "cnpj_raiz", "cnae_classe",
    "cnae_subclasse", "idade"
  ))
  d[, cnpj_raiz := clean_root(cnpj_raiz)]
  d <- d[cnpj_raiz %chin% roots & suppressWarnings(as.integer(ativo)) == 1L]
  d[, `:=`(
    escolaridade_n = suppressWarnings(as.integer(escolaridade)),
    sexo_n = suppressWarnings(as.integer(sexo)),
    raca_n = suppressWarnings(as.integer(raca)),
    salario_n = num_br(salario_medio),
    tempo_n = num_br(tempo_emprego),
    idade_n = suppressWarnings(as.numeric(idade))
  )]

  annual[[as.character(yr)]] <- d[, .(
    emprego = .N,
    salario_medio = if (any(is.finite(salario_n) & salario_n > 0))
      mean(salario_n[is.finite(salario_n) & salario_n > 0]) else NA_real_,
    prop_superior = if (any(escolaridade_n %in% 1:11))
      mean(escolaridade_n[escolaridade_n %in% 1:11] >= 9) else NA_real_,
    tempo_medio_trabalhadores = if (any(is.finite(tempo_n)))
      mean(tempo_n, na.rm = TRUE) else NA_real_,
    idade_media_trabalhadores = if (any(is.finite(idade_n)))
      mean(idade_n, na.rm = TRUE) else NA_real_,
    prop_homens = if (any(sexo_n %in% c(1L, 2L)))
      mean(sexo_n[sexo_n %in% c(1L, 2L)] == 1L) else NA_real_,
    prop_pretos = if (any(raca_n %in% c(1L, 2L, 4L, 6L, 8L)))
      mean(raca_n[raca_n %in% c(1L, 2L, 4L, 6L, 8L)] == 4L) else NA_real_,
    municipio_principal = mode_value(municipio),
    tamanho_estab_principal = mode_value(tamanho),
    cnae_classe_principal = mode_value(cnae_classe),
    cnae_subclasse_principal = mode_value(cnae_subclasse)
  ), by = cnpj_raiz]
  annual[[as.character(yr)]][, `:=`(
    ano = yr,
    uf_principal = substr(municipio_principal, 1L, 2L),
    division = substr(cnae_classe_principal, 1L, 2L),
    log_emprego = log1p(emprego),
    log_salario_medio = log(salario_medio)
  )]
  setorder(annual[[as.character(yr)]], cnpj_raiz)
  fwrite(annual[[as.character(yr)]], checkpoint, sep = ";", bom = TRUE)
  message(
    "RAIS ", yr, ": ", nrow(annual[[as.character(yr)]]),
    " CNPJs com vinculo ativo."
  )
  rm(d)
  gc(FALSE)
}

result <- rbindlist(annual, fill = TRUE)
setorder(result, cnpj_raiz, ano)
fwrite(result, output_file, sep = ";", bom = TRUE)
message("Caracteristicas RAIS salvas em: ", output_file)
