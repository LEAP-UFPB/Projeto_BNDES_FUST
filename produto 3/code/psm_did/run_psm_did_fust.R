#!/usr/bin/env Rscript

packages <- c(
  "data.table", "readxl", "stringr", "writexl",
  "MatchIt", "did", "ggplot2", "synthdid"
)
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
  library(writexl)
  library(MatchIt)
  library(did)
  library(ggplot2)
  library(synthdid)
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

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- args_all[grepl("^--file=", args_all)]
script_path <- if (length(script_arg) > 0) {
  normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
} else {
  normalizePath("produto 3/code/psm_did/run_psm_did_fust.R", winslash = "/", mustWork = TRUE)
}
produto_dir <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(produto_dir, ".."), winslash = "/", mustWork = TRUE)

rais_dir <- arg_value("rais", "D:/rais")
out_dir <- arg_value("out", file.path(produto_dir, "analysis", "psm_did"))
logs_dir <- file.path(produto_dir, "analysis", "psm_did", "logs")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

panel_fust_file <- arg_value(
  "panel-fust",
  file.path(repo_root, "data", "Painel_CNPJ_FUST_RAIS_2018_2024.xlsx")
)
max_controls_per_stratum <- as.integer(arg_value("max-controls-per-stratum", "5000"))
ratio_matches <- as.integer(arg_value("ratio", "3"))
caliper_ps <- as.numeric(arg_value("caliper", "0.10"))
match_years <- parse_years(arg_value("match-years", "2022:2024"))
outcome_years <- parse_years(arg_value("outcome-years", "2021:2025"))
treatment_years <- parse_years(arg_value("treatment-years", "2023:2025"))
rebuild_covariates <- tolower(arg_value("rebuild-covariates", "false")) %in% c("1", "true", "t", "yes", "y")
rebuild_outcomes <- tolower(arg_value("rebuild-outcomes", "false")) %in% c("1", "true", "t", "yes", "y")
covars_file <- file.path(out_dir, "covariaveis_matching_firma_ano_v2_sem_outcomes.csv")
panel_did_file <- file.path(out_dir, "painel_did_psm_rais_2021_2025_v2.csv")

log_file <- file.path(logs_dir, paste0("run_psm_did_fust_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Inicio: ", Sys.time())
message("Repo: ", repo_root)
message("RAIS: ", rais_dir)
message("Saida: ", out_dir)
message("Anos matching: ", paste(match_years, collapse = ", "))
message("Anos outcomes: ", paste(outcome_years, collapse = ", "))
message("Coortes tratadas: ", paste(treatment_years, collapse = ", "))

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

clean_cnpj_root <- function(cnpj_raiz, cnpj = NULL) {
  root <- clean_digits(cnpj_raiz)
  out <- rep(NA_character_, length(root))
  ok_root <- !is.na(root) & str_length(root) == 8
  out[ok_root] <- root[ok_root]
  if (!is.null(cnpj)) {
    cnpj_digits <- clean_digits(cnpj)
    ok_cnpj <- is.na(out) & !is.na(cnpj_digits) & str_length(cnpj_digits) >= 8
    out[ok_cnpj] <- str_sub(cnpj_digits[ok_cnpj], 1, 8)
  }
  ok_short <- is.na(out) & !is.na(root) & str_length(root) < 8
  out[ok_short] <- str_pad(root[ok_short], 8, pad = "0")
  out
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

parse_year_br <- function(x) {
  x <- as.character(x)
  year <- str_extract(x, "(19|20)\\d{2}")
  suppressWarnings(as.integer(year))
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

regiao_from_uf <- function(uf) {
  fifelse(
    uf %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO"), "Norte",
    fifelse(
      uf %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"), "Nordeste",
      fifelse(
        uf %in% c("DF", "GO", "MT", "MS"), "Centro-Oeste",
        fifelse(uf %in% c("ES", "MG", "RJ", "SP"), "Sudeste",
          fifelse(uf %in% c("PR", "RS", "SC"), "Sul", NA_character_)
        )
      )
    )
  )
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

broad_sector <- function(subsetor) {
  s <- suppressWarnings(as.integer(clean_digits(subsetor)))
  fifelse(
    s == 25, "Agricultura",
    fifelse(
      s == 15, "Construcao",
      fifelse(
        s %in% 3:13, "Industria",
        fifelse(
          s %in% c(16, 17), "Comercio",
          fifelse(s == 21, "Servicos", fifelse(s == 20, "Transporte", "Outros"))
        )
      )
    )
  )
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

year_dir <- function(year) {
  dir <- file.path(rais_dir, paste0("RAIS_", year))
  if (dir.exists(dir)) dir else NA_character_
}

estab_file_for_year <- function(year) {
  dir <- year_dir(year)
  if (is.na(dir)) return(NA_character_)
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  files <- files[
    str_detect(basename(files), regex("estb|estab", ignore_case = TRUE)) &
      str_detect(basename(files), regex("\\.(txt|comt)$", ignore_case = TRUE))
  ]
  if (length(files) == 0) return(NA_character_)
  files <- files[order(
    !str_detect(basename(files), regex("COMT$", ignore_case = TRUE)),
    !str_detect(basename(files), regex("RAIS_ESTAB", ignore_case = TRUE)),
    basename(files)
  )]
  files[1]
}

vinc_files_for_year <- function(year) {
  dir <- year_dir(year)
  if (is.na(dir)) return(character())
  list.files(
    dir,
    full.names = TRUE,
    recursive = FALSE,
    pattern = "(?i)^RAIS_VINC.*\\.(txt|comt)$"
  )
}

detect_estab_cols <- function(file) {
  header <- fread(file, nrows = 0, encoding = "Latin-1", showProgress = FALSE)
  cols <- names(header)
  list(
    cnpj = pick_col(cols, c("^CNPJ CEI$", "^CNPJ / CEI$"), required = FALSE),
    cnpj_raiz = pick_col(cols, c("^CNPJ RAIZ")),
    municipio = pick_col(cols, c("^MUNICIPIO CODIGO$", "^MUNICIPIO$")),
    empregados = pick_col(cols, c("^QTD VINCULOS ATIVOS$")),
    data_abertura = pick_col(cols, c("^DATA ABERTURA"), required = FALSE),
    tamanho_estab = pick_col(cols, c("^TAMANHO ESTABELECIMENTO"), required = FALSE),
    cnae_classe = pick_col(cols, c("^CNAE 2 0 CLASSE", "^CNAE 95 CLASSE"), required = FALSE),
    cnae_subclasse = pick_col(cols, c("^CNAE 2 0 SUBCLASSE"), required = FALSE),
    subsetor_ibge = pick_col(cols, c("^IBGE SUBSETOR"))
  )
}

detect_vinc_cols <- function(file) {
  header <- fread(file, nrows = 0, encoding = "Latin-1", showProgress = FALSE)
  cols <- names(header)
  list(
    cnpj = pick_col(cols, c("^CNPJ CEI$", "^CNPJ / CEI$"), required = FALSE),
    cnpj_raiz = pick_col(cols, c("^CNPJ RAIZ")),
    vinculo_ativo = pick_col(cols, c("VINCULO ATIVO 31 12")),
    rem_media = pick_col(cols, c("^VL REM MEDIA NOM$", "^VL REMUN MEDIA NOM$")),
    rem_dezembro = pick_col(cols, c("^VL REM DEZEMBRO NOM$", "^VL REMUN DEZEMBRO NOM$"), required = FALSE),
    grau_instr = pick_col(cols, c("^ESCOLARIDADE APOS 2005", "^GRAU INSTRUCAO"), required = FALSE),
    tempo_emprego = pick_col(cols, c("^TEMPO EMPREGO"), required = FALSE),
    idade = pick_col(cols, c("^IDADE$"), required = FALSE)
  )
}

read_selected <- function(file, columns) {
  columns <- columns[!is.na(unlist(columns))]
  dt <- fread(
    file,
    select = unique(unlist(columns)),
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

process_estab_year <- function(year) {
  file <- estab_file_for_year(year)
  if (is.na(file)) {
    stop("Sem arquivo de estabelecimentos para ", year, call. = FALSE)
  }
  message("Processando RAIS estabelecimentos ", year, ": ", basename(file))
  cols <- detect_estab_cols(file)
  dt <- read_selected(file, cols)
  dt[, cnpj_raiz := clean_cnpj_root(cnpj_raiz, if ("cnpj" %in% names(dt)) cnpj else NULL)]
  dt <- dt[!is.na(cnpj_raiz) & cnpj_raiz != "00000000"]
  dt[, ano := year]
  dt[, total_empregados_linha := parse_number_br(empregados)]
  dt[, uf := uf_from_municipio(municipio)]
  dt[, municipio := str_pad(clean_digits(municipio), 6, pad = "0")]
  dt[, subsetor_ibge := str_pad(clean_digits(subsetor_ibge), 2, pad = "0")]
  if (!"data_abertura" %in% names(dt)) dt[, data_abertura := NA_character_]
  if (!"tamanho_estab" %in% names(dt)) dt[, tamanho_estab := NA_character_]
  if (!"cnae_classe" %in% names(dt)) dt[, cnae_classe := NA_character_]
  if (!"cnae_subclasse" %in% names(dt)) dt[, cnae_subclasse := NA_character_]
  dt[, cnae_classe := clean_digits(cnae_classe)]
  dt[, cnae_subclasse := clean_digits(cnae_subclasse)]
  dt[, ano_abertura := parse_year_br(data_abertura)]
  dt[, tamanho_estab := str_pad(clean_digits(tamanho_estab), 2, pad = "0")]

  base <- dt[
    ,
    .(
      ano_abertura_min = suppressWarnings(as.numeric(min(ano_abertura, na.rm = TRUE)))
    ),
    by = .(ano, cnpj_raiz)
  ]
  base[is.infinite(ano_abertura_min), ano_abertura_min := NA_integer_]
  base[, idade_firma_pre := pmax(ano - ano_abertura_min, 0)]

  uf <- dt[
    ,
    .(empregados_municipio = sum(total_empregados_linha, na.rm = TRUE)),
    by = .(ano, cnpj_raiz, uf, municipio)
  ][order(ano, cnpj_raiz, -empregados_municipio, uf, municipio)]
  uf <- uf[, .SD[1], by = .(ano, cnpj_raiz)]

  setor <- dt[
    ,
    .(empregados_setor = sum(total_empregados_linha, na.rm = TRUE)),
    by = .(ano, cnpj_raiz, cnae_classe, cnae_subclasse, subsetor_ibge, tamanho_estab)
  ][order(ano, cnpj_raiz, -empregados_setor, subsetor_ibge, tamanho_estab)]
  setor <- setor[, .SD[1], by = .(ano, cnpj_raiz)]

  out <- merge(
    base,
    uf[, .(ano, cnpj_raiz, uf_principal = uf, municipio_principal = municipio)],
    by = c("ano", "cnpj_raiz"),
    all.x = TRUE
  )
  out <- merge(
    out,
    setor[, .(
      ano,
      cnpj_raiz,
      cnae_classe_principal = cnae_classe,
      cnae_subclasse_principal = cnae_subclasse,
      subsetor_ibge_principal_codigo = subsetor_ibge,
      tamanho_estab_principal = tamanho_estab
    )],
    by = c("ano", "cnpj_raiz"),
    all.x = TRUE
  )
  message("Ano ", year, ": firmas=", nrow(out))
  rm(dt)
  gc()
  out
}

load_fust_entities <- function() {
  message("Lendo painel FUST x RAIS: ", panel_fust_file)
  panel <- as.data.table(read_excel(panel_fust_file, sheet = "painel_cnpj_ano", col_types = "text"))
  panel[, cnpj_raiz := clean_cnpj_root(cnpj_raiz)]
  panel[, g := as.integer(ano_primeira_operacao_fust)]
  entity <- panel[
    !is.na(cnpj_raiz) & !is.na(g),
    .(
      cliente_fust = cliente_fust[which.max(!is.na(cliente_fust))],
      g = min(g, na.rm = TRUE),
      valor_operacao_fust = suppressWarnings(max(parse_number_br(valor_operacao_fust), na.rm = TRUE)),
      porte_fust = paste(sort(unique(porte_fust[!is.na(porte_fust)])), collapse = " | "),
      setor_fust = paste(sort(unique(setor_cnae_fust[!is.na(setor_cnae_fust)])), collapse = " | ")
    ),
    by = cnpj_raiz
  ]
  entity[is.infinite(valor_operacao_fust), valor_operacao_fust := NA_real_]
  entity
}

make_covariates <- function(universe, fust_entities) {
  setorder(universe, cnpj_raiz, ano)
  universe[, regiao := regiao_from_uf(uf_principal)]
  universe[, broad_sector := broad_sector(subsetor_ibge_principal_codigo)]
  universe[, ano_base := ano]
  universe[, g := ano + 1L]
  covars <- universe[g %in% treatment_years]
  covars <- merge(
    covars,
    fust_entities[, .(cnpj_raiz, g_fust = g, cliente_fust)],
    by = "cnpj_raiz",
    all.x = TRUE
  )
  covars[, treat := as.integer(!is.na(g_fust) & g_fust == g)]
  covars
}

preselect_controls <- function(treated_g, controls_g) {
  controls_g <- controls_g[
    !is.na(municipio_principal) & !is.na(broad_sector) & !is.na(tamanho_estab_principal)
  ]
  treated_g <- treated_g[
    !is.na(municipio_principal) & !is.na(broad_sector) & !is.na(tamanho_estab_principal)
  ]
  if (nrow(treated_g) == 0 || nrow(controls_g) == 0) {
    return(list(treated = treated_g[0], controls = controls_g[0]))
  }

  treated_g[, exact_key := paste(municipio_principal, broad_sector, tamanho_estab_principal, sep = "|")]
  controls_g[, exact_key := paste(municipio_principal, broad_sector, tamanho_estab_principal, sep = "|")]
  treated_keys <- unique(treated_g$exact_key)
  controls_g <- controls_g[exact_key %in% treated_keys]

  centers <- treated_g[, .(center_idade_firma = mean(idade_firma_pre, na.rm = TRUE)), by = exact_key]
  controls_g <- merge(controls_g, centers, by = "exact_key", all.x = TRUE)
  controls_g[, dist_center := abs(idade_firma_pre - center_idade_firma)]
  setorder(controls_g, exact_key, dist_center, cnpj_raiz)
  controls_g <- controls_g[, head(.SD, max_controls_per_stratum), by = exact_key]
  controls_g[, c("center_idade_firma", "dist_center") := NULL]
  list(treated = treated_g, controls = controls_g)
}

make_matching_pool <- function(covars, fust_entities) {
  fust_roots <- unique(fust_entities$cnpj_raiz)
  parts <- list()
  for (gg in treatment_years) {
    treated_g <- copy(covars[g == gg & treat == 1])
    controls_g <- copy(covars[g == gg & treat == 0 & !(cnpj_raiz %in% fust_roots)])
    pools <- preselect_controls(treated_g, controls_g)
    parts[[as.character(gg)]] <- rbindlist(list(pools$treated, pools$controls), fill = TRUE)
  }
  rbindlist(parts, fill = TRUE)
}

process_worker_covariates_file <- function(file, year, roots) {
  message("Processando covariaveis de trabalhadores ", year, ": ", basename(file))
  cols <- detect_vinc_cols(file)
  dt <- read_selected(file, cols)
  dt[, cnpj_raiz := clean_cnpj_root(cnpj_raiz, if ("cnpj" %in% names(dt)) cnpj else NULL)]
  dt <- dt[cnpj_raiz %in% roots]
  if (nrow(dt) == 0) {
    rm(dt)
    gc()
    return(data.table())
  }
  dt[, ano_base := year]
  dt[, ativo_31_12 := parse_number_br(vinculo_ativo) == 1]
  dt <- dt[ativo_31_12 == TRUE]
  if (!"grau_instr" %in% names(dt)) dt[, grau_instr := NA_character_]
  if (!"tempo_emprego" %in% names(dt)) dt[, tempo_emprego := NA_character_]
  if (!"idade" %in% names(dt)) dt[, idade := NA_character_]
  dt[, grau_instr_num := parse_number_br(grau_instr)]
  dt[, superior := as.integer(grau_instr_num >= 8 & grau_instr_num <= 11)]
  dt[, tempo_emprego_num := parse_number_br(tempo_emprego)]
  dt[, idade_num := parse_number_br(idade)]
  out <- dt[
    ,
    .(
      prop_superior = mean(superior, na.rm = TRUE),
      tempo_medio_trabalhadores = mean(tempo_emprego_num, na.rm = TRUE),
      idade_media_trabalhadores = mean(idade_num, na.rm = TRUE),
      trabalhadores_covariaveis = .N
    ),
    by = .(ano_base, cnpj_raiz)
  ]
  rm(dt)
  gc()
  out
}

build_worker_covariates <- function(pool) {
  roots <- unique(pool$cnpj_raiz)
  years <- sort(unique(pool$ano_base))
  parts <- list()
  for (yy in years) {
    files <- vinc_files_for_year(yy)
    if (length(files) == 0) {
      warning("Sem arquivos RAIS_VINC para covariaveis em ", yy)
      next
    }
    year_dt <- rbindlist(
      lapply(files, process_worker_covariates_file, year = yy, roots = roots),
      fill = TRUE
    )
    if (nrow(year_dt) > 0) {
      year_dt <- year_dt[
        ,
        .(
          prop_superior = weighted.mean(prop_superior, trabalhadores_covariaveis, na.rm = TRUE),
          tempo_medio_trabalhadores = weighted.mean(tempo_medio_trabalhadores, trabalhadores_covariaveis, na.rm = TRUE),
          idade_media_trabalhadores = weighted.mean(idade_media_trabalhadores, trabalhadores_covariaveis, na.rm = TRUE),
          trabalhadores_covariaveis = sum(trabalhadores_covariaveis, na.rm = TRUE)
        ),
        by = .(ano_base, cnpj_raiz)
      ]
      parts[[as.character(yy)]] <- year_dt
    }
    rm(year_dt)
    gc()
  }
  rbindlist(parts, fill = TRUE)
}

impute_matching_covariates <- function(dt) {
  num_vars <- c(
    "idade_firma_pre", "prop_superior",
    "tempo_medio_trabalhadores", "idade_media_trabalhadores"
  )
  for (v in num_vars) {
    dt[, (v) := as.numeric(get(v))]
    dt[, (v) := fifelse(
      is.na(get(v)) | !is.finite(get(v)),
      median(get(v), na.rm = TRUE),
      get(v)
    ), by = exact_key]
    global_med <- median(dt[[v]], na.rm = TRUE)
    dt[is.na(get(v)) | !is.finite(get(v)), (v) := global_med]
  }
  dt
}

run_matching <- function(covars, fust_entities) {
  fust_roots <- unique(fust_entities$cnpj_raiz)
  matched_parts <- list()
  input_parts <- list()
  diagnostics <- list()

  for (gg in treatment_years) {
    message("Matching coorte ", gg)
    treated_g <- copy(covars[g == gg & treat == 1])
    controls_g <- copy(covars[g == gg & treat == 0 & !(cnpj_raiz %in% fust_roots)])
    pools <- preselect_controls(treated_g, controls_g)
    treated_g <- pools$treated
    controls_g <- pools$controls
    if (nrow(treated_g) == 0 || nrow(controls_g) == 0) {
      warning("Coorte ", gg, " sem tratados ou controles elegiveis apos pre-selecao.")
      next
    }

    dt <- rbindlist(list(treated_g, controls_g), fill = TRUE)
    dt <- dt[!is.na(subsetor_ibge_principal_codigo)]
    dt[, treat := as.integer(treat)]
    dt <- impute_matching_covariates(dt)
    dt <- dt[
      is.finite(idade_firma_pre) &
        is.finite(prop_superior) &
        is.finite(tempo_medio_trabalhadores) &
        is.finite(idade_media_trabalhadores)
    ]
    input_parts[[as.character(gg)]] <- copy(dt)

    message(
      "Coorte ", gg, ": tratados=", sum(dt$treat == 1),
      " controles candidatos=", sum(dt$treat == 0),
      " estratos=", uniqueN(dt$exact_key)
    )

    match_formula <- treat ~ idade_firma_pre + prop_superior +
      tempo_medio_trabalhadores + idade_media_trabalhadores
    m <- tryCatch(
      matchit(
        formula = match_formula,
        data = dt,
        method = "nearest",
        distance = "glm",
        exact = ~ municipio_principal + broad_sector + tamanho_estab_principal,
        replace = TRUE,
        ratio = ratio_matches,
        caliper = caliper_ps,
        std.caliper = TRUE
      ),
      error = function(e) {
        warning("Fallback matching coorte ", gg, ": ", conditionMessage(e))
        tryCatch(
          matchit(
            formula = treat ~ idade_firma_pre + prop_superior +
              tempo_medio_trabalhadores + idade_media_trabalhadores,
            data = dt, method = "nearest", distance = "glm",
            exact = ~ broad_sector + tamanho_estab_principal,
            replace = TRUE, ratio = ratio_matches,
            caliper = caliper_ps, std.caliper = TRUE
          ),
          error = function(e2) {
            warning("Fallback sem caliper coorte ", gg, ": ", conditionMessage(e2))
            matchit(
              formula = treat ~ idade_firma_pre + prop_superior +
                tempo_medio_trabalhadores + idade_media_trabalhadores,
              data = dt, method = "nearest", distance = "glm",
              exact = ~ broad_sector + tamanho_estab_principal,
              replace = TRUE, ratio = 1
            )
          }
        )
      }
    )

    md <- as.data.table(match.data(m, drop.unmatched = TRUE))
    if (!"weights" %in% names(md)) {
      weight_candidates <- grep("^weights", names(md), value = TRUE)
      if (length(weight_candidates) > 0) {
        setnames(md, weight_candidates[1], "weights")
      } else {
        md[, weights := 1]
      }
    }
    md[, coorte_match := gg]
    matched_parts[[as.character(gg)]] <- md
    diagnostics[[as.character(gg)]] <- data.table(
      coorte = gg,
      tratados_entrada = sum(dt$treat == 1),
      controles_candidatos = sum(dt$treat == 0),
      tratados_pareados = sum(md$treat == 1 & md$weights > 0),
      controles_pareados = sum(md$treat == 0 & md$weights > 0),
      controles_unicos_pareados = uniqueN(md[treat == 0 & weights > 0, cnpj_raiz]),
      ratio = ratio_matches,
      caliper = caliper_ps,
      max_controls_per_stratum = max_controls_per_stratum
    )
    rm(dt, m, md)
    gc()
  }

  list(
    matched = rbindlist(matched_parts, fill = TRUE),
    inputs = rbindlist(input_parts, fill = TRUE),
    diagnostics = rbindlist(diagnostics, fill = TRUE)
  )
}

wmean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

wvar <- function(x, w) {
  m <- wmean(x, w)
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (!any(ok) || is.na(m)) return(NA_real_)
  sum(w[ok] * (x[ok] - m)^2) / sum(w[ok])
}

balance_numeric_one <- function(dt, stage, weight_col) {
  vars <- c(
    "idade_firma_pre", "prop_superior",
    "tempo_medio_trabalhadores", "idade_media_trabalhadores"
  )
  rbindlist(lapply(vars, function(v) {
    x_t <- dt[treat == 1, get(v)]
    x_c <- dt[treat == 0, get(v)]
    w_t <- dt[treat == 1, get(weight_col)]
    w_c <- dt[treat == 0, get(weight_col)]
    mt <- wmean(x_t, w_t)
    mc <- wmean(x_c, w_c)
    sd_pool <- sqrt((wvar(x_t, w_t) + wvar(x_c, w_c)) / 2)
    data.table(
      etapa = stage,
      tipo = "numerica",
      variavel = v,
      media_tratadas = mt,
      media_controles = mc,
      diferenca = mt - mc,
      smd_abs = abs((mt - mc) / sd_pool)
    )
  }), fill = TRUE)
}

balance_categorical_one <- function(dt, stage, weight_col) {
  vars <- c(
    "municipio_principal", "broad_sector",
    "tamanho_estab_principal", "subsetor_ibge_principal_codigo"
  )
  rbindlist(lapply(vars, function(v) {
    cats <- sort(unique(dt[[v]][!is.na(dt[[v]])]))
    if (length(cats) == 0) return(data.table())
    diffs <- vapply(cats, function(cat) {
      wt <- dt[treat == 1, sum(get(weight_col) * as.integer(get(v) == cat), na.rm = TRUE)]
      wc <- dt[treat == 0, sum(get(weight_col) * as.integer(get(v) == cat), na.rm = TRUE)]
      st <- dt[treat == 1, sum(get(weight_col), na.rm = TRUE)]
      sc <- dt[treat == 0, sum(get(weight_col), na.rm = TRUE)]
      (wt / st) - (wc / sc)
    }, numeric(1))
    data.table(
      etapa = stage,
      tipo = "categorica",
      variavel = v,
      media_tratadas = NA_real_,
      media_controles = NA_real_,
      diferenca = 0.5 * sum(abs(diffs), na.rm = TRUE),
      smd_abs = max(abs(diffs), na.rm = TRUE)
    )
  }), fill = TRUE)
}

make_balance <- function(inputs, matched) {
  inputs[, peso_balance := 1]
  matched[, peso_balance := weights]
  rbindlist(
    list(
      balance_numeric_one(inputs, "Antes do matching", "peso_balance"),
      balance_categorical_one(inputs, "Antes do matching", "peso_balance"),
      balance_numeric_one(matched[weights > 0], "Depois do matching", "peso_balance"),
      balance_categorical_one(matched[weights > 0], "Depois do matching", "peso_balance")
    ),
    fill = TRUE
  )
}

process_vinc_file <- function(file, year, roots) {
  message("Processando vinculos ", year, ": ", basename(file))
  cols <- detect_vinc_cols(file)
  dt <- read_selected(file, cols)
  dt[, cnpj_raiz := clean_cnpj_root(cnpj_raiz, if ("cnpj" %in% names(dt)) cnpj else NULL)]
  dt <- dt[cnpj_raiz %in% roots]
  if (nrow(dt) == 0) {
    rm(dt)
    gc()
    return(data.table())
  }
  dt[, ano := year]
  dt[, ativo_31_12 := parse_number_br(vinculo_ativo) == 1]
  dt[, rem_media_nom := parse_number_br(rem_media)]
  if (!"rem_dezembro" %in% names(dt)) dt[, rem_dezembro := NA_character_]
  dt[, rem_dezembro_nom := parse_number_br(rem_dezembro)]
  out <- dt[
    ,
    .(
      total_empregados = sum(ativo_31_12, na.rm = TRUE),
      massa_salarial_media = sum(fifelse(ativo_31_12, rem_media_nom, 0), na.rm = TRUE),
      massa_salarial_dezembro = sum(fifelse(ativo_31_12, rem_dezembro_nom, 0), na.rm = TRUE),
      vinculos_rais = .N
    ),
    by = .(ano, cnpj_raiz)
  ]
  rm(dt)
  gc()
  out
}

build_outcome_panel <- function(units) {
  roots <- unique(units$cnpj_raiz)
  parts <- list()
  for (yy in outcome_years) {
    files <- vinc_files_for_year(yy)
    if (length(files) == 0) {
      warning("Sem arquivos RAIS_VINC para ", yy)
      next
    }
    year_dt <- rbindlist(lapply(files, process_vinc_file, year = yy, roots = roots), fill = TRUE)
    if (nrow(year_dt) > 0) {
      year_dt <- year_dt[
        ,
        .(
          total_empregados = sum(total_empregados, na.rm = TRUE),
          massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
          massa_salarial_dezembro = sum(massa_salarial_dezembro, na.rm = TRUE),
          vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
        ),
        by = .(ano, cnpj_raiz)
      ]
      parts[[as.character(yy)]] <- year_dt
    }
    rm(year_dt)
    gc()
  }
  outcomes <- rbindlist(parts, fill = TRUE)
  grid <- CJ(cnpj_raiz = roots, ano = outcome_years, unique = TRUE)
  panel <- merge(grid, outcomes, by = c("cnpj_raiz", "ano"), all.x = TRUE)
  fill_cols <- c("total_empregados", "massa_salarial_media", "massa_salarial_dezembro", "vinculos_rais")
  for (col in fill_cols) {
    set(panel, which(is.na(panel[[col]])), col, 0)
  }
  panel <- merge(panel, units, by = "cnpj_raiz", all.x = TRUE)
  panel[, log_emprego := log1p(total_empregados)]
  panel[, log_massa_salarial := log1p(massa_salarial_media)]
  panel[, id_num := as.integer(factor(unit_id))]
  panel[]
}

run_did <- function(panel, outcome, label, file_stub) {
  message("Estimando Callaway-Sant'Anna por pilha de coorte para ", label)
  cohort_results <- lapply(sort(unique(panel$coorte_match)), function(cohort) {
    panel_cohort <- copy(panel[coorte_match == cohort])
    panel_cohort[, `:=`(g = as.numeric(g), y = get(outcome))]
    att <- att_gt(
      yname = "y", tname = "ano", idname = "id_num", gname = "g",
      weightsname = "match_weight", data = panel_cohort, panel = TRUE,
      control_group = "nevertreated", xformla = ~ 1,
      base_period = "universal", clustervars = "id_num",
      bstrap = TRUE, biters = 999, cband = FALSE,
      print_details = FALSE
    )
    data.table(
      outcome = label, grupo = att$group, ano = att$t,
      att = att$att, se = att$se,
      tratados_coorte = uniqueN(panel_cohort[treat == 1, cnpj_raiz]),
      pretest_pvalue = att$Wpval
    )
  })
  gt <- rbindlist(cohort_results, fill = TRUE)
  gt[, ci_low := att - 1.96 * se]
  gt[, ci_high := att + 1.96 * se]
  gt[, event_time := ano - grupo]
  dyn <- gt[, {
    valid <- is.finite(att) & is.finite(se) & tratados_coorte > 0
    weights <- tratados_coorte[valid] / sum(tratados_coorte[valid])
    .(
      att = sum(weights * att[valid]),
      se = sqrt(sum((weights * se[valid])^2)),
      coortes = sum(valid),
      tratados = sum(tratados_coorte[valid])
    )
  }, by = .(outcome, event_time)]
  dyn[is.na(se) & !is.na(att) & abs(att) < .Machine$double.eps^0.5, se := 0]
  dyn[, ci_low := att - 1.96 * se]
  dyn[, ci_high := att + 1.96 * se]
  dyn[is.na(ci_low) & !is.na(att) & abs(att) < .Machine$double.eps^0.5, `:=`(
    ci_low = 0,
    ci_high = 0
  )]
  x_breaks <- seq(min(dyn$event_time, na.rm = TRUE), max(dyn$event_time, na.rm = TRUE), by = 1)

  post <- gt[event_time >= 0 & is.finite(att) & is.finite(se)]
  post_weights <- post$tratados_coorte / sum(post$tratados_coorte)
  summary_dt <- data.table(
    outcome = label,
    tipo = "ATT agregado por coortes pareadas",
    att = sum(post_weights * post$att),
    se = sqrt(sum((post_weights * post$se)^2))
  )
  summary_dt[, `:=`(ci_low = att - 1.96 * se, ci_high = att + 1.96 * se)]

  p <- ggplot(dyn, aes(x = event_time, y = att)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -1, color = "grey60", linewidth = 0.35, linetype = "dashed") +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8FB8CA", alpha = 0.25) +
    geom_line(color = "#1C5D73", linewidth = 0.7) +
    geom_point(color = "#1C5D73", size = 2) +
    scale_x_continuous(breaks = x_breaks) +
    labs(
      title = paste0("Event study - ", label),
      x = "Tempo relativo ao primeiro apoio FUST",
      y = "ATT"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold"),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey20"),
      panel.grid.minor = element_blank()
    )

  ggsave(
    filename = file.path(out_dir, paste0("event_study_", file_stub, ".png")),
    plot = p,
    width = 8,
    height = 5,
    dpi = 300,
    bg = "white"
  )

  list(simple = summary_dt, group_time = gt, dynamic = dyn)
}

run_did_notyet <- function(panel, fust_entities, outcome, label) {
  units <- unique(panel[, .(cnpj_raiz, match_weight)])
  units <- units[, .(match_weight = max(match_weight)), by = cnpj_raiz]
  units <- merge(units, fust_entities[, .(cnpj_raiz, g_actual = g)], by = "cnpj_raiz", all.x = TRUE)
  units[is.na(g_actual) | g_actual > max(outcome_years), g_actual := 0L]
  outcomes <- unique(panel[, .(cnpj_raiz, ano, y = get(outcome))], by = c("cnpj_raiz", "ano"))
  data <- merge(outcomes, units, by = "cnpj_raiz")
  data[, id_notyet := as.integer(factor(cnpj_raiz))]
  att <- att_gt(
    yname = "y", tname = "ano", idname = "id_notyet", gname = "g_actual",
    weightsname = "match_weight", data = data, panel = TRUE,
    control_group = "notyettreated", xformla = ~ 1,
    base_period = "universal", clustervars = "id_notyet",
    bstrap = TRUE, biters = 999, cband = FALSE,
    print_details = FALSE
  )
  dynamic <- aggte(att, type = "dynamic")
  data.table(
    outcome = label, event_time = dynamic$egt, att = dynamic$att.egt,
    se = dynamic$se.egt,
    ci_low = dynamic$att.egt - 1.96 * dynamic$se.egt,
    ci_high = dynamic$att.egt + 1.96 * dynamic$se.egt,
    pretest_pvalue = as.numeric(att$Wpval)
  )
}

run_synthdid <- function(panel, outcome, label) {
  synthdid_se <- function(estimate) {
    placebo_se <- tryCatch(
      as.numeric(sqrt(vcov(estimate, method = "placebo"))),
      error = function(e) NA_real_
    )
    if (is.finite(placebo_se)) return(placebo_se)
    tryCatch(
      as.numeric(sqrt(vcov(estimate, method = "jackknife"))),
      error = function(e) NA_real_
    )
  }
  rbindlist(lapply(sort(unique(panel$coorte_match)), function(cohort) {
    dt <- unique(
      panel[coorte_match == cohort, .(unit_id, ano, treat, y = get(outcome))],
      by = c("unit_id", "ano")
    )
    if (uniqueN(dt[treat == 1, unit_id]) < 2 || uniqueN(dt[treat == 0, unit_id]) < 2) {
      return(data.table(outcome = label, coorte = cohort, status = "amostra_insuficiente"))
    }
    wide <- dcast(dt, unit_id + treat ~ ano, value.var = "y")
    setorder(wide, treat, unit_id)
    years <- as.integer(setdiff(names(wide), c("unit_id", "treat")))
    year_cols <- as.character(years)
    Y <- as.matrix(wide[, ..year_cols])
    N0 <- sum(wide$treat == 0)
    T0 <- sum(years < cohort)
    estimate <- synthdid_estimate(Y, N0, T0)
    se <- synthdid_se(estimate)

    placebo_estimate <- NA_real_
    placebo_se <- NA_real_
    if (T0 >= 2) {
      Y_placebo <- Y[, years < cohort, drop = FALSE]
      placebo <- synthdid_estimate(Y_placebo, N0, ncol(Y_placebo) - 1L)
      placebo_estimate <- as.numeric(placebo)
      placebo_se <- synthdid_se(placebo)
    }
    data.table(
      outcome = label, coorte = cohort, att = as.numeric(estimate), se = se,
      ci_low = as.numeric(estimate) - 1.96 * se,
      ci_high = as.numeric(estimate) + 1.96 * se,
      placebo_pre = placebo_estimate, placebo_se = placebo_se,
      placebo_pvalue = 2 * pnorm(-abs(placebo_estimate / placebo_se)),
      controles = N0, tratadas = nrow(wide) - N0, periodos_pre = T0,
      status = "estimado"
    )
  }), fill = TRUE)
}

fmt_num <- function(x, digits = 3) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ","))
}

escape_tex <- function(x) {
  x <- as.character(x)
  x <- str_replace_all(x, "\\\\", "\\\\textbackslash{}")
  x <- str_replace_all(x, "([#$%&_{}])", "\\\\\\1")
  x
}

safe_write_xlsx <- function(x, path) {
  tryCatch(
    write_xlsx(x, path),
    error = function(e) {
      warning(
        "Nao foi possivel sobrescrever ", path,
        ". Feche o arquivo se ele estiver aberto. Motivo: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
}

write_tex <- function(summary_info, balance, att_summary) {
  bal_after <- balance[etapa == "Depois do matching"]
  setorder(bal_after, -smd_abs)
  bal_show <- head(bal_after, 12)
  att_show <- copy(att_summary)

  bal_rows <- apply(bal_show, 1, function(row) {
    paste0(
      escape_tex(row[["variavel"]]), " & ",
      escape_tex(row[["tipo"]]), " & ",
      fmt_num(as.numeric(row[["diferenca"]])), " & ",
      fmt_num(as.numeric(row[["smd_abs"]])), " \\\\"
    )
  })
  att_rows <- apply(att_show, 1, function(row) {
    paste0(
      escape_tex(row[["outcome"]]), " & ",
      fmt_num(as.numeric(row[["att"]])), " & ",
      fmt_num(as.numeric(row[["se"]])), " & ",
      fmt_num(as.numeric(row[["ci_low"]])), " & ",
      fmt_num(as.numeric(row[["ci_high"]])), " \\\\"
    )
  })

  tex <- c(
    "\\documentclass[12pt]{article}",
    "\\usepackage[utf8]{inputenc}",
    "\\usepackage[T1]{fontenc}",
    "\\usepackage[brazil]{babel}",
    "\\usepackage{booktabs}",
    "\\usepackage{graphicx}",
    "\\usepackage{float}",
    "\\usepackage[margin=2.5cm]{geometry}",
    "\\title{PSM e Diferenças-em-Diferenças Escalonado: BNDES FUST e RAIS}",
    "\\author{}",
    "\\date{\\today}",
    "\\begin{document}",
    "\\maketitle",
    "\\section{Objetivo}",
    paste0(
      "Esta nota documenta o pareamento por escore de propensão (PSM) e a estimação ",
      "de diferenças-em-diferenças escalonado para firmas apoiadas pelo BNDES FUST, ",
      "comparadas a contrafactuais selecionados no universo da RAIS."
    ),
    "\\section{Dados}",
    paste0(
      "A base de tratamento parte do painel FUST--RAIS 2018--2024 já existente no projeto. ",
      "Como a RAIS cruzada anterior não continha 2025, os microdados originais em \\texttt{",
      escape_tex(rais_dir), "} foram relidos. O painel de desfechos usa os anos ",
      min(outcome_years), "--", max(outcome_years), ", incluindo os arquivos RAIS 2025 em formato \\texttt{.COMT}."
    ),
    paste0(
      "Foram consideradas como tratadas as coortes ",
      paste(treatment_years, collapse = ", "),
      ". Firmas com primeiro apoio em 2026 foram excluídas da estimação principal por não possuírem período pós-tratamento observado na RAIS 2025."
    ),
    "\\section{Pareamento por PSM}",
    paste0(
      "O matching foi feito separadamente por coorte, usando como ano-base o ano imediatamente anterior ao primeiro apoio. ",
      "O pool de controle exclui qualquer CNPJ raiz presente na base FUST. Antes do PSM, os controles foram restringidos aos mesmos estratos ",
      "de município principal, setor amplo e tamanho RAIS do estabelecimento principal observados entre as tratadas. Dentro de cada estrato, mantiveram-se até ",
      max_controls_per_stratum, " controles mais próximos em idade da firma para preservar comparabilidade sem abandonar o universo amplo da RAIS."
    ),
    paste0(
      "O modelo logit do escore de propensão incluiu: idade da firma, proporção de trabalhadores com ensino superior, ",
      "tempo médio de emprego dos trabalhadores, idade média dos trabalhadores e dummies do subsetor IBGE principal. ",
      "O pareamento nearest-neighbor usou reposição, razão ",
      ratio_matches, ":1, caliper de ", caliper_ps,
      " desvio-padrão do escore e pareamento exato em município principal, setor amplo e tamanho RAIS do estabelecimento principal."
    ),
    "\\section{Balanceamento}",
    paste0(
      "A Tabela \\ref{tab:balance} mostra o balanceamento após o matching. Para variáveis numéricas, a última coluna reporta ",
      "a diferença média padronizada absoluta. Para variáveis categóricas, reporta a maior diferença absoluta de proporções entre categorias; ",
      "a coluna de diferença corresponde à distância de variação total."
    ),
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{Balanceamento após o PSM}",
    "\\label{tab:balance}",
    "\\begin{tabular}{llrr}",
    "\\toprule",
    "Variável & Tipo & Diferença & Medida abs. \\\\",
    "\\midrule",
    bal_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\section{Diferenças-em-Diferenças Escalonado}",
    paste0(
      "Os efeitos foram estimados com o método de Callaway e Sant'Anna no pacote \\texttt{did}. ",
      "A variável de grupo é o ano do primeiro apoio FUST e os controles são firmas nunca apoiadas no FUST dentro da janela de estimação. ",
      "A especificação usa \\texttt{base\\_period = ``universal''}, isto é, baseline universal para a construção dos efeitos dinâmicos. ",
      "Os pesos do PSM entram como pesos amostrais. Os desfechos são \\(\\log(1+emprego)\\) e \\(\\log(1+massa\\ salarial)\\), ",
      "onde massa salarial é a soma da remuneração média nominal dos vínculos ativos em 31/12."
    ),
    "\\begin{table}[H]",
    "\\centering",
    "\\caption{ATT agregado}",
    "\\label{tab:att}",
    "\\begin{tabular}{lrrrr}",
    "\\toprule",
    "Desfecho & ATT & Erro-padrão & IC 95\\% inf. & IC 95\\% sup. \\\\",
    "\\midrule",
    att_rows,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}",
    "\\section{Event studies}",
    "\\begin{figure}[H]",
    "\\centering",
    "\\includegraphics[width=0.9\\textwidth]{event_study_log_emprego.png}",
    "\\caption{Event study para log de emprego}",
    "\\end{figure}",
    "\\begin{figure}[H]",
    "\\centering",
    "\\includegraphics[width=0.9\\textwidth]{event_study_log_massa_salarial.png}",
    "\\caption{Event study para log de massa salarial}",
    "\\end{figure}",
    "\\section{Resumo da amostra}",
    paste0(
      "Tratadas pareadas: ", summary_info$treated_matched,
      ". Controles únicos pareados: ", summary_info$control_matched,
      ". Firmas no painel DID: ", summary_info$panel_units,
      ". Linhas firma-ano: ", summary_info$panel_rows, "."
    ),
    "\\end{document}"
  )
  writeLines(tex, file.path(out_dir, "metodologia_psm_did.tex"), useBytes = TRUE)
}

fust_entities <- load_fust_entities()
fust_entities <- fust_entities[g %in% c(treatment_years, 2026L)]
message("Firmas FUST com chave: ", nrow(fust_entities))
message("Distribuicao por coorte FUST:")
print(fust_entities[, .N, by = g][order(g)])

if (file.exists(covars_file) && !rebuild_covariates) {
  message("Lendo covariaveis pre-tratamento em cache: ", covars_file)
  covars <- fread(
    covars_file,
    sep = ";",
    encoding = "UTF-8",
    colClasses = c(
      cnpj_raiz = "character",
      uf_principal = "character",
      municipio_principal = "character",
      cnae_classe_principal = "character",
      cnae_subclasse_principal = "character",
      subsetor_ibge_principal_codigo = "character",
      tamanho_estab_principal = "character",
      regiao = "character",
      broad_sector = "character",
      cliente_fust = "character"
    ),
    showProgress = TRUE
  )
  covars[, cnpj_raiz := clean_cnpj_root(cnpj_raiz)]
} else {
  message("Construindo universo de covariaveis pre-tratamento")
  universe <- rbindlist(lapply(match_years, process_estab_year), fill = TRUE)
  covars_estab <- make_covariates(universe, fust_entities)
  pool_estab <- make_matching_pool(covars_estab, fust_entities)
  worker_covars <- build_worker_covariates(pool_estab)
  covars <- merge(pool_estab, worker_covars, by = c("ano_base", "cnpj_raiz"), all.x = TRUE)
  fwrite(covars, covars_file, sep = ";", bom = TRUE, na = "")
}

match_res <- run_matching(covars, fust_entities)
matched <- match_res$matched
inputs <- match_res$inputs
diagnostics <- match_res$diagnostics

if (nrow(matched) == 0) {
  stop("Matching nao retornou unidades pareadas.", call. = FALSE)
}

balance <- make_balance(inputs, matched)

matched_units <- matched[
  weights > 0,
  .(
    treat = max(treat, na.rm = TRUE),
    g = ifelse(max(treat, na.rm = TRUE) == 1, min(g[treat == 1], na.rm = TRUE), 0L),
    match_weight = sum(weights, na.rm = TRUE),
    idade_firma_pre = weighted.mean(idade_firma_pre, weights, na.rm = TRUE),
    prop_superior = weighted.mean(prop_superior, weights, na.rm = TRUE),
    tempo_medio_trabalhadores = weighted.mean(tempo_medio_trabalhadores, weights, na.rm = TRUE),
    idade_media_trabalhadores = weighted.mean(idade_media_trabalhadores, weights, na.rm = TRUE),
    uf_principal = first(uf_principal),
    municipio_principal = first(municipio_principal),
    broad_sector = first(broad_sector),
    tamanho_estab_principal = first(tamanho_estab_principal),
    subsetor_ibge_principal_codigo = first(subsetor_ibge_principal_codigo)
  ),
  by = .(cnpj_raiz, coorte_match)
]
matched_units[!is.finite(g), g := 0L]
matched_units[treat == 0, g := 0L]
matched_units[match_weight <= 0 | is.na(match_weight), match_weight := 1]
matched_units[, unit_id := paste(coorte_match, cnpj_raiz, sep = "_")]

message("Unidades pareadas: ", nrow(matched_units))
message("Tratadas pareadas: ", nrow(matched_units[treat == 1]))
message("Controles unicos pareados: ", nrow(matched_units[treat == 0]))

safe_write_xlsx(
  list(
    diagnostico_matching = diagnostics,
    unidades_pareadas = matched_units,
    matching_linhas = matched,
    balanceamento = balance
  ),
  file.path(out_dir, "psm_matching_balanceamento.xlsx")
)
fwrite(diagnostics, file.path(out_dir, "diagnostico_matching.csv"), sep = ";", bom = TRUE, na = "")
fwrite(matched_units, file.path(out_dir, "unidades_pareadas.csv"), sep = ";", bom = TRUE, na = "")
fwrite(balance, file.path(out_dir, "balanceamento_psm.csv"), sep = ";", bom = TRUE, na = "")

if (file.exists(panel_did_file) && !rebuild_outcomes) {
  message("Lendo painel de outcomes em cache: ", panel_did_file)
  panel_did <- fread(
    panel_did_file,
    sep = ";",
    encoding = "UTF-8",
    colClasses = c(
      cnpj_raiz = "character",
      coortes_match = "character",
      uf_principal = "character",
      municipio_principal = "character",
      broad_sector = "character",
      tamanho_estab_principal = "character",
      subsetor_ibge_principal_codigo = "character"
    ),
    showProgress = TRUE
  )
  outcome_cols <- c(
    "cnpj_raiz", "ano", "total_empregados", "massa_salarial_media",
    "massa_salarial_dezembro", "vinculos_rais"
  )
  outcomes_cached <- unique(panel_did[, ..outcome_cols], by = c("cnpj_raiz", "ano"))
  panel_did <- merge(outcomes_cached, matched_units, by = "cnpj_raiz", allow.cartesian = TRUE)
  panel_did[, `:=`(
    log_emprego = log1p(total_empregados),
    log_massa_salarial = log1p(massa_salarial_media),
    id_num = as.integer(factor(unit_id))
  )]
} else {
  message("Construindo painel de outcomes RAIS")
  panel_did <- build_outcome_panel(matched_units)
  fwrite(panel_did, panel_did_file, sep = ";", bom = TRUE, na = "")
}

did_emp <- run_did(panel_did, "log_emprego", "Log emprego", "log_emprego")
did_mass <- run_did(panel_did, "log_massa_salarial", "Log massa salarial", "log_massa_salarial")
notyet_emp <- run_did_notyet(panel_did, fust_entities, "log_emprego", "Log emprego")
notyet_mass <- run_did_notyet(panel_did, fust_entities, "log_massa_salarial", "Log massa salarial")
att_notyet <- rbindlist(list(notyet_emp, notyet_mass), fill = TRUE)
synthdid_results <- rbindlist(list(
  run_synthdid(panel_did, "log_emprego", "Log emprego"),
  run_synthdid(panel_did, "log_massa_salarial", "Log massa salarial")
), fill = TRUE)

att_summary <- rbindlist(list(did_emp$simple, did_mass$simple), fill = TRUE)
att_group_time <- rbindlist(list(did_emp$group_time, did_mass$group_time), fill = TRUE)
att_dynamic <- rbindlist(list(did_emp$dynamic, did_mass$dynamic), fill = TRUE)
pretrend_diagnostic <- unique(
  att_group_time[
    !is.na(pretest_pvalue.V1),
    .(
      outcome,
      coorte = grupo,
      p_value = pretest_pvalue.V1,
      aprovado_5pct = pretest_pvalue.V1 >= 0.05
    )
  ]
)

safe_write_xlsx(
  list(
    att_agregado = att_summary,
    att_grupo_tempo = att_group_time,
    event_study = att_dynamic,
    did_not_yet_treated = att_notyet,
    synthetic_did = synthdid_results
  ),
  file.path(out_dir, "att_callaway_santanna.xlsx")
)
fwrite(att_summary, file.path(out_dir, "att_agregado.csv"), sep = ";", bom = TRUE, na = "")
fwrite(att_group_time, file.path(out_dir, "att_grupo_tempo.csv"), sep = ";", bom = TRUE, na = "")
fwrite(att_dynamic, file.path(out_dir, "att_event_study.csv"), sep = ";", bom = TRUE, na = "")
fwrite(att_notyet, file.path(out_dir, "att_event_study_not_yet_treated.csv"), sep = ";", bom = TRUE, na = "")
fwrite(synthdid_results, file.path(out_dir, "synthetic_did_por_coorte.csv"), sep = ";", bom = TRUE, na = "")
fwrite(pretrend_diagnostic, file.path(out_dir, "diagnostico_pretrend.csv"), sep = ";", bom = TRUE, na = "")
if (any(pretrend_diagnostic$aprovado_5pct == FALSE, na.rm = TRUE)) {
  warning(
    "PRE-TREND REPROVADO: os resultados nao devem ser interpretados como causais. ",
    "Consulte diagnostico_pretrend.csv.",
    call. = FALSE
  )
}

summary_info <- list(
  treated_matched = nrow(matched_units[treat == 1]),
  control_matched = nrow(matched_units[treat == 0]),
  panel_units = uniqueN(panel_did$cnpj_raiz),
  panel_rows = nrow(panel_did)
)
write_tex(summary_info, balance, att_summary)

message("Fim: ", Sys.time())
message("Log salvo em: ", log_file)
