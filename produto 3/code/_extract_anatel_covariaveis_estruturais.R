#!/usr/bin/env Rscript

# Covariaveis operacionais da Anatel sem usar o numero de acessos: cobertura
# territorial, diversidade de tecnologias/ofertas, velocidade anunciada e
# porte cadastral. As colunas mensais de acessos nao sao lidas.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(FALSE)
script <- normalizePath(
  sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/"
)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_file <- file.path(data_dir, "produto3_covariaveis_estruturais_anatel.csv")

clean_root <- function(x) {
  z <- gsub("\\D", "", as.character(x))
  z[nchar(z) < 8L] <- sprintf("%08d", suppressWarnings(as.integer(z[nchar(z) < 8L])))
  substr(z, 1L, 8L)
}

normalize_text <- function(x) {
  toupper(iconv(as.character(x), from = "", to = "ASCII//TRANSLIT"))
}

num_br <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

source_files <- c(
  `2021` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2021_Colunas.csv"),
  `2022` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2022_Colunas.csv"),
  `2023` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2023_Colunas.csv"),
  `2024` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2024_Colunas.csv")
)

annual <- list()
for (yr_chr in names(source_files)) {
  checkpoint <- file.path(
    data_dir, paste0("produto3_covariaveis_estruturais_anatel_", yr_chr, ".csv")
  )
  if (file.exists(checkpoint)) {
    annual[[yr_chr]] <- fread(
      checkpoint, sep = ";", colClasses = c(cnpj_raiz = "character")
    )
    message("Checkpoint carregado: ", basename(checkpoint))
    next
  }

  message("Lendo estrutura Anatel ", yr_chr, "...")
  d <- fread(
    source_files[[yr_chr]], select = 1:13,
    colClasses = "character", encoding = "UTF-8", showProgress = FALSE
  )
  setnames(d, c(
    "cnpj", "velocidade", "municipio", "uf", "faixa_velocidade",
    "tecnologia", "empresa", "porte", "tipo_pessoa", "tipo_produto",
    "codigo_ibge", "grupo_economico", "meio_acesso"
  ))
  d[, `:=`(
    cnpj_raiz = clean_root(cnpj),
    velocidade_n = num_br(velocidade),
    tecnologia_n = normalize_text(tecnologia),
    meio_n = normalize_text(meio_acesso),
    porte_n = normalize_text(porte),
    pessoa_n = normalize_text(tipo_pessoa)
  )]
  d <- d[grepl("^[0-9]{8}$", cnpj_raiz)]
  d[, oferta_id := paste(
    codigo_ibge, faixa_velocidade, tecnologia, tipo_pessoa,
    tipo_produto, meio_acesso, sep = "|"
  )]

  annual[[yr_chr]] <- d[, .(
    n_municipios = uniqueN(codigo_ibge[nzchar(codigo_ibge)]),
    n_ufs = uniqueN(uf[nzchar(uf)]),
    n_tecnologias = uniqueN(tecnologia[nzchar(tecnologia)]),
    n_meios_acesso = uniqueN(meio_acesso[nzchar(meio_acesso)]),
    n_faixas_velocidade = uniqueN(faixa_velocidade[nzchar(faixa_velocidade)]),
    n_tipos_produto = uniqueN(tipo_produto[nzchar(tipo_produto)]),
    n_ofertas_catalogo = uniqueN(oferta_id),
    velocidade_mediana = if (any(is.finite(velocidade_n)))
      median(unique(velocidade_n[is.finite(velocidade_n)])) else 0,
    velocidade_maxima = if (any(is.finite(velocidade_n)))
      max(velocidade_n, na.rm = TRUE) else 0,
    porte_pequeno = as.integer(any(grepl("PEQUENO", porte_n))),
    oferece_pessoa_juridica = as.integer(any(grepl("JURIDICA", pessoa_n))),
    oferece_fibra = as.integer(any(grepl("FIBRA|FTTH|FTTX", paste(tecnologia_n, meio_n)))),
    oferece_radio = as.integer(any(grepl("RADIO|WI-FI|WIFI", paste(tecnologia_n, meio_n)))),
    oferece_cabo = as.integer(any(grepl("CABO|ETHERNET|HFC", paste(tecnologia_n, meio_n)))),
    oferece_satelite = as.integer(any(grepl("SATEL", paste(tecnologia_n, meio_n))))
  ), by = cnpj_raiz]
  annual[[yr_chr]][, ano := as.integer(yr_chr)]
  setcolorder(annual[[yr_chr]], c("cnpj_raiz", "ano"))
  fwrite(annual[[yr_chr]], checkpoint, sep = ";", bom = TRUE)
  message("Anatel ", yr_chr, ": ", nrow(annual[[yr_chr]]), " CNPJs.")
  rm(d)
  gc(FALSE)
}

result <- rbindlist(annual, fill = TRUE)
setorder(result, cnpj_raiz, ano)
fwrite(result, output_file, sep = ";", bom = TRUE)
message("Covariaveis estruturais salvas em: ", output_file)
