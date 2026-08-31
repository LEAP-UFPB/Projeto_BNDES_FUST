#!/usr/bin/env Rscript

# Extrai emprego e salario medio da RAIS apenas para prestadoras observadas na
# base da Anatel. O resultado e um cache usado pelo matching de banda larga.

suppressPackageStartupMessages(library(data.table))

args <- commandArgs(FALSE)
script <- normalizePath(
  sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/"
)
repo <- normalizePath(file.path(dirname(script), "..", ".."), winslash = "/")
data_dir <- file.path(repo, "data")
rais_dir <- Sys.getenv("RAIS_DIR", unset = "D:/rais")
output_file <- file.path(data_dir, "produto3_covariaveis_rais_matching_banda_larga.csv")
employment_history_file <- file.path(
  data_dir, "produto3_emprego_rais_matching_banda_larga.csv"
)

clean_root <- function(x) {
  sprintf("%08d", suppressWarnings(as.integer(gsub("\\D", "", as.character(x)))))
}

panel <- fread(
  file.path(data_dir, "produto3_painel_banda_larga_fixa.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
roots <- unique(clean_root(panel[observada_no_ano == TRUE, cnpj_raiz]))
roots <- roots[grepl("^[0-9]{8}$", roots)]
annual <- list()
for (yr in 2023:2024) {
  folder <- file.path(rais_dir, paste0("RAIS_", yr))
  checkpoint <- file.path(
    data_dir, paste0("produto3_covariaveis_rais_matching_banda_larga_", yr, ".csv")
  )
  if (file.exists(checkpoint)) {
    annual[[as.character(yr)]] <- fread(
      checkpoint, sep = ";", colClasses = c(cnpj_raiz = "character")
    )
    message("Checkpoint carregado: ", basename(checkpoint))
    next
  }

  files <- list.files(folder, pattern = "^RAIS_VINC.*\\.(txt|COMT)$", full.names = TRUE)
  parts <- list()
  for (f in files) {
    message("Lendo ", basename(f), " (", round(file.info(f)$size / 1024^3, 2), " GB)...")
    d <- fread(
      f, select = c(37L, 3L, 23L), colClasses = "character",
      showProgress = FALSE
    )
    setnames(d, c("cnpj_raiz", "ativo", "rem_media"))
    d[, cnpj_raiz := clean_root(cnpj_raiz)]
    d[, `:=`(
      ativo = suppressWarnings(as.integer(ativo)),
      rem_media = suppressWarnings(as.numeric(rem_media))
    )]
    d <- d[cnpj_raiz %chin% roots & ativo == 1L]
    parts[[basename(f)]] <- d[, .(
      emprego = .N,
      rem_sum = sum(rem_media[is.finite(rem_media) & rem_media > 0]),
      rem_n = sum(is.finite(rem_media) & rem_media > 0)
    ), by = cnpj_raiz]
    rm(d)
    gc(FALSE)
  }
  annual[[as.character(yr)]] <- rbindlist(parts)[, .(
    emprego = sum(emprego), rem_sum = sum(rem_sum), rem_n = sum(rem_n)
  ), by = cnpj_raiz]
  annual[[as.character(yr)]][, `:=`(
    salario_medio = fifelse(rem_n > 0, rem_sum / rem_n, NA_real_), ano = yr
  )]
  annual[[as.character(yr)]] <- annual[[as.character(yr)]][, .(
    cnpj_raiz, ano, emprego, salario_medio
  )]
  fwrite(annual[[as.character(yr)]], checkpoint, sep = ";", bom = TRUE)
  message("RAIS ", yr, ": ", nrow(annual[[as.character(yr)]]), " raizes agregadas.")
}

result <- rbindlist(annual)
result[, `:=`(
  log_emprego = log1p(emprego),
  log_salario_medio = log(salario_medio)
)]
setorder(result, cnpj_raiz, ano)
fwrite(result, output_file, sep = ";", bom = TRUE)
message("Cache salvo: ", output_file)
message("Linhas: ", nrow(result), "; raizes: ", uniqueN(result$cnpj_raiz))

# Historico adicional de emprego. Os anos 2023--2024 ja foram apurados nos
# microdados de vinculos acima. Para 2021--2022, os arquivos de
# estabelecimentos sao muito menores e a soma de vinculos ativos reproduz a
# mesma medida de emprego no fim do ano. Esse historico permite parear por
# nivel e trajetoria pre-FUST sem usar o outcome da Anatel.
establishment_files <- c(
  `2021` = file.path(rais_dir, "RAIS_2021", "Estb2021ID.txt"),
  `2022` = file.path(rais_dir, "RAIS_2022", "RAIS_ESTAB2022_ID.txt.txt")
)
employment_history <- list()
for (yr_chr in names(establishment_files)) {
  checkpoint <- file.path(
    data_dir, paste0("produto3_emprego_rais_matching_banda_larga_", yr_chr, ".csv")
  )
  if (file.exists(checkpoint)) {
    employment_history[[yr_chr]] <- fread(
      checkpoint, sep = ";", colClasses = c(cnpj_raiz = "character")
    )
    message("Checkpoint carregado: ", basename(checkpoint))
    next
  }

  f <- establishment_files[[yr_chr]]
  if (!file.exists(f)) stop("Arquivo RAIS de estabelecimentos ausente: ", f)
  message("Lendo emprego dos estabelecimentos RAIS ", yr_chr, "...")
  d <- fread(
    f, select = c(5L, 20L),
    colClasses = "character", showProgress = FALSE
  )
  setnames(d, c("cnpj_raiz", "emprego"))
  d[, `:=`(
    cnpj_raiz = clean_root(cnpj_raiz),
    emprego = suppressWarnings(as.numeric(emprego))
  )]
  d <- d[cnpj_raiz %chin% roots & is.finite(emprego)]
  d <- d[, .(emprego = sum(emprego)), by = cnpj_raiz]
  d[, ano := as.integer(yr_chr)]
  setcolorder(d, c("cnpj_raiz", "ano", "emprego"))
  fwrite(d, checkpoint, sep = ";", bom = TRUE)
  employment_history[[yr_chr]] <- d
  rm(d)
  gc(FALSE)
  message("RAIS ", yr_chr, ": emprego de ", nrow(employment_history[[yr_chr]]), " raizes.")
}

employment_history[["2023"]] <- result[ano == 2023L, .(cnpj_raiz, ano, emprego)]
employment_history[["2024"]] <- result[ano == 2024L, .(cnpj_raiz, ano, emprego)]
employment_history <- merge(
  CJ(cnpj_raiz = roots, ano = 2021:2024),
  rbindlist(employment_history), by = c("cnpj_raiz", "ano"), all.x = TRUE
)
employment_history[is.na(emprego), emprego := 0]
employment_history[, log_emprego := log1p(emprego)]
setorder(employment_history, cnpj_raiz, ano)
fwrite(employment_history, employment_history_file, sep = ";", bom = TRUE)
message("Historico de emprego salvo: ", employment_history_file)
