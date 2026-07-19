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

repo_root <- normalizePath(file.path(getwd()), winslash = "/", mustWork = TRUE)
fust_file <- arg_value(
  "fust",
  file.path(repo_root, "data", "Base de Operacoes_FUST_compilada.xlsx")
)
if (!file.exists(fust_file)) {
  fust_file <- file.path(repo_root, "data", "Base de Operações_FUST_compilada.xlsx")
}

rais_dir <- arg_value("rais", "D:/rais")
rais2_dir <- arg_value("rais2", "D:/rais2")
output_dir <- arg_value("out", file.path(repo_root, "analysis", "outputs", "rais_fust"))
years <- parse_years(arg_value("anos", "2018:2024"))
panel_file <- arg_value(
  "panel-out",
  file.path(
    repo_root,
    "data",
    paste0("Painel_CNPJ_FUST_RAIS_", min(years), "_", max(years), ".xlsx")
  )
)
only_panel_xlsx <- tolower(arg_value("only-panel-xlsx", "false")) %in% c(
  "1", "true", "t", "yes", "y"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(panel_file), recursive = TRUE, showWarnings = FALSE)

normalize_text <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- str_to_upper(x)
  x <- str_replace_all(x, "[^A-Z0-9]+", " ")
  x <- str_squish(x)
  x[x == ""] <- NA_character_
  x
}

tokenize_name <- function(x) {
  stopwords <- c(
    "A", "AS", "DA", "DAS", "DE", "DO", "DOS", "E", "EM", "ME", "S", "SA",
    "S A", "LTDA", "EIRELI", "EPP", "MEI"
  )
  tokens <- str_split(normalize_text(x), "\\s+")
  lapply(tokens, function(value) {
    value <- value[!is.na(value)]
    value <- value[!value %in% stopwords]
    value[str_length(value) >= 2]
  })
}

name_similarity <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    return(0)
  }

  if (a == b) {
    return(1)
  }

  if (str_detect(a, fixed(b)) || str_detect(b, fixed(a))) {
    return(0.95)
  }

  a_tokens <- tokenize_name(a)[[1]]
  b_tokens <- tokenize_name(b)[[1]]
  if (length(a_tokens) == 0 || length(b_tokens) == 0) {
    return(0)
  }

  length(intersect(a_tokens, b_tokens)) / length(unique(a_tokens))
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
  ok8 <- !is.na(x) & str_length(x) == 8
  out[ok8] <- suppressWarnings(as.integer(str_sub(x[ok8], 5, 8)))
  out
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

subsetor_ibge_label <- function(code) {
  labels <- c(
    "01" = "Extrativa mineral",
    "02" = "Produtos minerais nao metalicos",
    "03" = "Industria metalurgica",
    "04" = "Industria mecanica",
    "05" = "Eletrico e comunicacoes",
    "06" = "Material de transporte",
    "07" = "Madeira e mobiliario",
    "08" = "Papel, papelao, editorial e grafica",
    "09" = "Borracha, fumo e couros",
    "10" = "Industria quimica",
    "11" = "Industria textil",
    "12" = "Industria de calcados",
    "13" = "Alimentos e bebidas",
    "14" = "Servicos de utilidade publica",
    "15" = "Construcao civil",
    "16" = "Comercio varejista",
    "17" = "Comercio atacadista",
    "18" = "Instituicoes financeiras",
    "19" = "Administracao tecnica profissional",
    "20" = "Transporte e comunicacoes",
    "21" = "Alojamento, alimentacao e reparacao",
    "22" = "Medico, odontologico e veterinario",
    "23" = "Ensino",
    "24" = "Administracao publica",
    "25" = "Agricultura"
  )
  unname(labels[code])
}

collapse_unique <- function(x) {
  values <- sort(unique(as.character(x[!is.na(x) & as.character(x) != ""])))
  if (length(values) == 0) {
    return(NA_character_)
  }
  paste(values, collapse = " | ")
}

sum_or_na <- function(x) {
  values <- parse_number_br(x)
  if (all(is.na(values))) {
    return(NA_real_)
  }
  sum(values, na.rm = TRUE)
}

extract_year <- function(x) {
  x <- as.character(x)
  match <- str_extract(x, "(19|20)\\d{2}")
  suppressWarnings(as.integer(match))
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

detect_columns <- function(file, kind) {
  header <- fread(
    file,
    nrows = 0,
    encoding = "Latin-1",
    showProgress = FALSE
  )
  cols <- names(header)

  common <- list(
    cnpj = pick_col(cols, c("^CNPJ CEI$", "^CNPJ / CEI$")),
    cnpj_raiz = pick_col(cols, c("^CNPJ RAIZ$")),
    razao_social = pick_col(cols, c("^RAZAO SOCIAL$"), required = FALSE),
    municipio = pick_col(
      cols,
      c("^MUNICIPIO CODIGO$", "^MUNICIPIO$", "^MUN TRAB$", "^MUNICIPIO TRAB CODIGO$"),
      required = FALSE
    ),
    cnae_classe = pick_col(cols, c("^CNAE 2 0 CLASSE", "^CNAE 95 CLASSE"), required = FALSE),
    cnae_subclasse = pick_col(cols, c("^CNAE 2 0 SUBCLASSE"), required = FALSE),
    ibge_subsetor = pick_col(cols, c("^IBGE SUBSETOR"), required = FALSE)
  )

  if (kind == "estab") {
    common$qtd_vinculos_ativos <- pick_col(cols, c("^QTD VINCULOS ATIVOS$"), required = FALSE)
    common$qtd_vinculos_clt <- pick_col(cols, c("^QTD VINCULOS CLT$"), required = FALSE)
    common$qtd_vinculos_estatutarios <- pick_col(
      cols,
      c("^QTD VINCULOS ESTATUTARIOS$"),
      required = FALSE
    )
  }

  if (kind == "vinc") {
    common$vinculo_ativo <- pick_col(cols, c("VINCULO ATIVO 31 12"), required = FALSE)
    common$motivo_desligamento <- pick_col(cols, c("^MOTIVO DESLIGAMENTO"), required = FALSE)
    common$mes_desligamento <- pick_col(cols, c("^MES DESLIGAMENTO"), required = FALSE)
    common$data_admissao <- pick_col(cols, c("^DATA ADMISSAO", "^DATA ADMISSAO DECLARADA"), required = FALSE)
    common$rem_media <- pick_col(cols, c("^VL REM MEDIA NOM$", "^VL REMUN MEDIA NOM$"), required = FALSE)
    common$rem_dezembro <- pick_col(cols, c("^VL REM DEZEMBRO NOM$", "^VL REMUN DEZEMBRO NOM$"), required = FALSE)
  }

  common[!is.na(unlist(common))]
}

read_selected <- function(file, columns) {
  selected <- unique(unlist(columns))
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
  candidates <- c(
    file.path(rais2_dir, paste0("RAIS_", year)),
    file.path(rais_dir, paste0("RAIS_", year))
  )
  candidates[file.exists(candidates)][1]
}

estab_file_for_year <- function(year) {
  dir <- year_dir(year)
  if (is.na(dir)) {
    return(NA_character_)
  }

  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  files <- files[
    str_detect(basename(files), regex("estb|estab", ignore_case = TRUE)) &
      str_detect(basename(files), regex("\\.(txt|comt)$", ignore_case = TRUE))
  ]

  if (length(files) == 0) {
    return(NA_character_)
  }

  files <- files[order(
    !str_detect(basename(files), regex("COMT$", ignore_case = TRUE)),
    !str_detect(basename(files), regex("RAIS_ESTAB", ignore_case = TRUE)),
    basename(files)
  )]
  files[1]
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

message("Lendo base FUST: ", fust_file)
fust <- as.data.table(read_excel(fust_file, sheet = 1, col_types = "text"))

required_fust <- c("cnpj", "cliente")
missing_fust <- setdiff(required_fust, names(fust))
if (length(missing_fust) > 0) {
  stop("Colunas ausentes na base FUST: ", paste(missing_fust, collapse = ", "), call. = FALSE)
}

fust[, operacao_id := .I]
fust[, cnpj_digits := clean_digits(cnpj)]
fust[, cnpj_completo := fifelse(!str_detect(cnpj, "\\*") & str_length(cnpj_digits) == 14, cnpj_digits, NA_character_)]
fust[, cnpj_raiz_fust := fifelse(!is.na(cnpj_completo), str_sub(cnpj_completo, 1, 8), NA_character_)]
fust[, cnpj_ordem_visivel := fifelse(str_detect(cnpj, "\\*") & str_length(cnpj_digits) > 4, str_sub(cnpj_digits, -4), NA_character_)]
fust[, cnpj_raiz_sufixo_visivel := fifelse(
  str_detect(cnpj, "\\*") & str_length(cnpj_digits) > 4,
  str_sub(cnpj_digits, 1, str_length(cnpj_digits) - 4),
  NA_character_
)]
fust[, cliente_norm := normalize_text(cliente)]

target_names <- sort(unique(fust[is.na(cnpj_raiz_fust) & !is.na(cliente_norm), cliente_norm]))
known_roots <- sort(unique(fust[!is.na(cnpj_raiz_fust), cnpj_raiz_fust]))

message("Operacoes FUST: ", nrow(fust))
message("CNPJ completo na planilha: ", sum(!is.na(fust$cnpj_completo)))
message("Clientes sem CNPJ completo para tentar resolver por nome: ", length(target_names))

resolve_year <- rev(sort(years))[1]
resolve_file <- estab_file_for_year(resolve_year)
if (is.na(resolve_file)) {
  stop("Nao achei arquivo de estabelecimentos para o ano ", resolve_year, call. = FALSE)
}

message("Resolvendo nomes com RAIS Estabelecimentos ", resolve_year, ": ", resolve_file)
estab_cols <- detect_columns(resolve_file, "estab")
estab_resolve <- read_selected(resolve_file, estab_cols)
estab_resolve[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
estab_resolve[, cnpj := str_pad(clean_digits(cnpj), 14, pad = "0")]
estab_resolve[, cnpj_ordem := str_sub(cnpj, 9, 12)]
estab_resolve[, razao_norm := normalize_text(razao_social)]

name_matches <- estab_resolve[
  (!is.na(cnpj_raiz) & cnpj_raiz %in% known_roots) |
    (!is.na(razao_norm) & razao_norm %in% target_names),
  .(
    cnpj_raiz,
    cnpj,
    razao_social,
    razao_norm,
    municipio,
    uf = uf_from_municipio(municipio),
    qtd_vinculos_ativos = parse_number_br(qtd_vinculos_ativos)
  )
]

resolved_by_name <- name_matches[
  razao_norm %in% target_names & !is.na(cnpj_raiz),
  .(
    cnpj_raiz_resolvido = fifelse(uniqueN(cnpj_raiz) == 1, first(cnpj_raiz), NA_character_),
    cnpjs_raiz_encontrados = paste(sort(unique(cnpj_raiz)), collapse = "; "),
    qtd_cnpjs_raiz = uniqueN(cnpj_raiz),
    estabelecimentos_encontrados = uniqueN(cnpj),
    max_vinculos_ativos_estab = max(qtd_vinculos_ativos, na.rm = TRUE)
  ),
  by = .(cliente_norm = razao_norm)
]
resolved_by_name[is.infinite(max_vinculos_ativos_estab), max_vinculos_ativos_estab := NA_real_]

masked_keys <- unique(fust[
  is.na(cnpj_raiz_fust) &
    !is.na(cliente_norm) &
    !is.na(cnpj_raiz_sufixo_visivel) &
    !is.na(cnpj_ordem_visivel),
  .(
    cliente_norm,
    cnpj_digits,
    cnpj_raiz_sufixo_visivel,
    cnpj_ordem_visivel,
    sufixo_len = str_length(cnpj_raiz_sufixo_visivel)
  )
])

masked_matches <- data.table()
if (nrow(masked_keys) > 0) {
  estab_key <- estab_resolve[
    !is.na(cnpj_raiz) & !is.na(cnpj_ordem),
    .(
      cnpj_raiz,
      cnpj,
      cnpj_ordem,
      razao_social,
      razao_norm,
      municipio,
      uf = uf_from_municipio(municipio),
      qtd_vinculos_ativos = parse_number_br(qtd_vinculos_ativos)
    )
  ]

  masked_matches <- rbindlist(lapply(sort(unique(masked_keys$sufixo_len)), function(len) {
    keys_len <- masked_keys[sufixo_len == len]
    estab_len <- copy(estab_key)
    estab_len[, cnpj_raiz_sufixo_visivel := str_sub(cnpj_raiz, 8 - len + 1, 8)]
    merge(
      keys_len,
      estab_len,
      by.x = c("cnpj_raiz_sufixo_visivel", "cnpj_ordem_visivel"),
      by.y = c("cnpj_raiz_sufixo_visivel", "cnpj_ordem"),
      allow.cartesian = TRUE
    )
  }), fill = TRUE)

  if (nrow(masked_matches) > 0) {
    masked_matches[, score_nome := mapply(name_similarity, cliente_norm, razao_norm)]
  }
}

resolved_by_mask <- if (nrow(masked_matches) > 0) {
  masked_matches[
    score_nome >= 0.5,
    .(
      melhor_score_nome = {
        max_score <- max(score_nome, na.rm = TRUE)
        max_score
      },
      cnpj_raiz_mascarado = {
        max_score <- max(score_nome, na.rm = TRUE)
        best_roots <- sort(unique(cnpj_raiz[score_nome == max_score]))
        if (length(best_roots) == 1) best_roots[1] else NA_character_
      },
      cnpjs_raiz_candidatos = paste(sort(unique(cnpj_raiz)), collapse = "; "),
      candidatos_cnpj_mascarado = .N
    ),
    by = .(cliente_norm, cnpj_digits)
  ]
} else {
  data.table(
    cliente_norm = character(),
    cnpj_digits = character(),
    cnpj_raiz_mascarado = character(),
    cnpjs_raiz_candidatos = character(),
    candidatos_cnpj_mascarado = integer(),
    melhor_score_nome = numeric()
  )
}

fust_keys <- merge(
  fust,
  resolved_by_name[, .(cliente_norm, cnpj_raiz_resolvido, qtd_cnpjs_raiz, cnpjs_raiz_encontrados)],
  by = "cliente_norm",
  all.x = TRUE
)
fust_keys <- merge(
  fust_keys,
  resolved_by_mask,
  by = c("cliente_norm", "cnpj_digits"),
  all.x = TRUE
)
fust_keys[, cnpj_raiz := fcoalesce(cnpj_raiz_fust, cnpj_raiz_resolvido, cnpj_raiz_mascarado)]
fust_keys[, status_chave := fifelse(
  !is.na(cnpj_raiz_fust),
  "cnpj_completo_planilha",
  fifelse(
    !is.na(cnpj_raiz_resolvido),
    "resolvido_por_razao_social_rais",
    fifelse(!is.na(cnpj_raiz_mascarado), "resolvido_por_cnpj_mascarado_nome", "sem_chave")
  )
)]
fust_keys[
  is.na(cnpj_raiz_fust) & qtd_cnpjs_raiz > 1 & is.na(cnpj_raiz_resolvido),
  status_chave := "nome_ambiguo_rais"
]
fust_keys[
  is.na(cnpj_raiz_fust) & qtd_cnpjs_raiz > 1 & is.na(cnpj_raiz_resolvido) & !is.na(cnpj_raiz_mascarado),
  status_chave := "resolvido_por_cnpj_mascarado_nome"
]

target_roots <- sort(unique(fust_keys[!is.na(cnpj_raiz), cnpj_raiz]))
if (length(target_roots) == 0) {
  stop("Nenhum CNPJ raiz encontrado para cruzar com a RAIS.", call. = FALSE)
}

message("CNPJ raiz para cruzamento RAIS: ", length(target_roots))
message("Operacoes sem chave RAIS: ", sum(is.na(fust_keys$cnpj_raiz)))

company_ref <- fust_keys[
  !is.na(cnpj_raiz),
  .(
    cliente_fust = paste(sort(unique(cliente)), collapse = " | "),
    operacoes_fust = .N,
    valor_operacao_fust = sum(parse_number_br(valor_operacao_r), na.rm = TRUE),
    valor_desembolsado_fust = sum(parse_number_br(valor_desembolsado_r), na.rm = TRUE)
  ),
  by = cnpj_raiz
]

process_estab_year <- function(year) {
  file <- estab_file_for_year(year)
  if (is.na(file)) {
    warning("Sem arquivo de estabelecimentos para ", year)
    return(data.table())
  }

  message("Processando estabelecimentos ", year, ": ", file)
  cols <- detect_columns(file, "estab")
  dt <- read_selected(file, cols)
  dt[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
  dt <- dt[cnpj_raiz %in% target_roots]
  if (nrow(dt) == 0) {
    return(data.table())
  }

  dt[, ano := year]
  dt[, cnpj := str_pad(clean_digits(cnpj), 14, pad = "0")]
  dt[, uf := uf_from_municipio(municipio)]
  dt[, qtd_vinculos_ativos := parse_number_br(qtd_vinculos_ativos)]
  dt[, qtd_vinculos_clt := parse_number_br(qtd_vinculos_clt)]
  dt[, qtd_vinculos_estatutarios := parse_number_br(qtd_vinculos_estatutarios)]

  dt[
    ,
    .(
      total_firmas = uniqueN(cnpj),
      total_empregados_estab = sum(qtd_vinculos_ativos, na.rm = TRUE),
      total_empregados_clt_estab = sum(qtd_vinculos_clt, na.rm = TRUE),
      total_empregados_estatutarios_estab = sum(qtd_vinculos_estatutarios, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, uf, setor_cnae_classe = cnae_classe, ibge_subsetor)
  ]
}

process_vinc_file <- function(file, year) {
  message("Processando vinculos ", year, ": ", basename(file))
  cols <- detect_columns(file, "vinc")
  dt <- read_selected(file, cols)
  dt[, cnpj_raiz := str_pad(clean_digits(cnpj_raiz), 8, pad = "0")]
  dt <- dt[cnpj_raiz %in% target_roots]
  if (nrow(dt) == 0) {
    return(data.table())
  }

  dt[, ano := year]
  dt[, cnpj := str_pad(clean_digits(cnpj), 14, pad = "0")]
  dt[, uf := uf_from_municipio(municipio)]
  dt[, ativo_31_12 := parse_number_br(vinculo_ativo) == 1]
  dt[, admissao_ano := parse_admission_year(data_admissao)]
  dt[, novo_emprego := admissao_ano == year]
  dt[, desligado := parse_number_br(mes_desligamento) > 0 | parse_number_br(motivo_desligamento) > 0]
  dt[, rem_media_nom := parse_number_br(rem_media)]
  dt[, rem_dezembro_nom := parse_number_br(rem_dezembro)]

  dt[
    ,
    .(
      total_empregados = sum(ativo_31_12, na.rm = TRUE),
      massa_salarial_media = sum(fifelse(ativo_31_12, rem_media_nom, NA_real_), na.rm = TRUE),
      massa_salarial_dezembro = sum(fifelse(ativo_31_12, rem_dezembro_nom, NA_real_), na.rm = TRUE),
      novos_empregos = sum(novo_emprego, na.rm = TRUE),
      desligamentos = sum(desligado, na.rm = TRUE),
      vinculos_rais = .N
    ),
    by = .(
      ano,
      cnpj_raiz,
      uf,
      setor_cnae_classe = cnae_classe,
      setor_cnae_subclasse = cnae_subclasse,
      ibge_subsetor
    )
  ]
}

estab_metrics <- rbindlist(lapply(years, process_estab_year), fill = TRUE)
vinc_metrics <- rbindlist(
  lapply(years, function(year) {
    files <- vinc_files_for_year(year)
    if (length(files) == 0) {
      warning("Sem arquivos de vinculos para ", year)
      return(data.table())
    }
    rbindlist(lapply(files, process_vinc_file, year = year), fill = TRUE)
  }),
  fill = TRUE
)

if (nrow(vinc_metrics) > 0) {
  vinc_metrics <- vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
      massa_salarial_dezembro = sum(massa_salarial_dezembro, na.rm = TRUE),
      novos_empregos = sum(novos_empregos, na.rm = TRUE),
      desligamentos = sum(desligamentos, na.rm = TRUE),
      vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, uf, setor_cnae_classe, setor_cnae_subclasse, ibge_subsetor)
  ]
  vinc_metrics[, rendimento_medio := fifelse(total_empregados > 0, massa_salarial_media / total_empregados, NA_real_)]
  vinc_metrics[, saldo_empregos := novos_empregos - desligamentos]
}

resumo_empresas <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
      massa_salarial_dezembro = sum(massa_salarial_dezembro, na.rm = TRUE),
      novos_empregos = sum(novos_empregos, na.rm = TRUE),
      desligamentos = sum(desligamentos, na.rm = TRUE),
      saldo_empregos = sum(saldo_empregos, na.rm = TRUE),
      vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz)
  ][
    ,
    rendimento_medio := fifelse(total_empregados > 0, massa_salarial_media / total_empregados, NA_real_)
  ]
} else {
  data.table()
}

firmas_empresas <- if (nrow(estab_metrics) > 0) {
  estab_metrics[
    ,
    .(
      total_firmas = sum(total_firmas, na.rm = TRUE),
      total_empregados_estab = sum(total_empregados_estab, na.rm = TRUE),
      total_empregados_clt_estab = sum(total_empregados_clt_estab, na.rm = TRUE),
      total_empregados_estatutarios_estab = sum(total_empregados_estatutarios_estab, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz)
  ]
} else {
  data.table()
}

if (nrow(resumo_empresas) > 0 && nrow(firmas_empresas) > 0) {
  resumo_empresas <- merge(resumo_empresas, firmas_empresas, by = c("ano", "cnpj_raiz"), all.x = TRUE)
}
if (nrow(resumo_empresas) > 0) {
  resumo_empresas <- merge(resumo_empresas, company_ref, by = "cnpj_raiz", all.x = TRUE)
  setorder(resumo_empresas, ano, -total_empregados)
}

ranking_empresas <- copy(resumo_empresas)
if (nrow(ranking_empresas) > 0) {
  ranking_empresas[, rank_empregos := frank(-total_empregados, ties.method = "dense"), by = ano]
  setcolorder(ranking_empresas, c("ano", "rank_empregos", "cnpj_raiz", "cliente_fust"))
}

emprego_setor <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
      novos_empregos = sum(novos_empregos, na.rm = TRUE),
      desligamentos = sum(desligamentos, na.rm = TRUE),
      saldo_empregos = sum(saldo_empregos, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, setor_cnae_classe, setor_cnae_subclasse, ibge_subsetor)
  ][
    ,
    rendimento_medio := fifelse(total_empregados > 0, massa_salarial_media / total_empregados, NA_real_)
  ]
} else {
  data.table()
}

emprego_uf <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
      novos_empregos = sum(novos_empregos, na.rm = TRUE),
      desligamentos = sum(desligamentos, na.rm = TRUE),
      saldo_empregos = sum(saldo_empregos, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, uf)
  ][
    ,
    rendimento_medio := fifelse(total_empregados > 0, massa_salarial_media / total_empregados, NA_real_)
  ]
} else {
  data.table()
}

# ---------------------------------------------------------------------------
# Painel balanceado empresa x ano
# ---------------------------------------------------------------------------

entity_found <- fust_keys[
  !is.na(cnpj_raiz),
  .(
    id_empresa_painel = paste0("CNPJ_RAIZ_", cnpj_raiz[1]),
    cnpj_fust = collapse_unique(cnpj),
    cliente_fust = collapse_unique(cliente),
    status_chave = collapse_unique(status_chave),
    qtd_operacoes_fust = .N,
    valor_operacao_fust = sum_or_na(valor_operacao_r),
    valor_desembolsado_fust = sum_or_na(valor_desembolsado_r),
    ano_primeira_operacao_fust = suppressWarnings(min(extract_year(data_da_contratacao), na.rm = TRUE)),
    ano_ultima_operacao_fust = suppressWarnings(max(extract_year(data_da_contratacao), na.rm = TRUE)),
    uf_fust = collapse_unique(uf),
    municipio_fust = collapse_unique(municipio),
    setor_cnae_fust = collapse_unique(setor_cnae),
    subsetor_cnae_fust = collapse_unique(subsetor_cnae_agrupado),
    porte_fust = collapse_unique(porte_do_cliente)
  ),
  by = cnpj_raiz
]
entity_found[is.infinite(ano_primeira_operacao_fust), ano_primeira_operacao_fust := NA_integer_]
entity_found[is.infinite(ano_ultima_operacao_fust), ano_ultima_operacao_fust := NA_integer_]

entity_unresolved <- fust_keys[
  is.na(cnpj_raiz),
  .(
    cnpj_fust = collapse_unique(cnpj),
    cliente_fust = collapse_unique(cliente),
    status_chave = collapse_unique(status_chave),
    qtd_operacoes_fust = .N,
    valor_operacao_fust = sum_or_na(valor_operacao_r),
    valor_desembolsado_fust = sum_or_na(valor_desembolsado_r),
    ano_primeira_operacao_fust = suppressWarnings(min(extract_year(data_da_contratacao), na.rm = TRUE)),
    ano_ultima_operacao_fust = suppressWarnings(max(extract_year(data_da_contratacao), na.rm = TRUE)),
    uf_fust = collapse_unique(uf),
    municipio_fust = collapse_unique(municipio),
    setor_cnae_fust = collapse_unique(setor_cnae),
    subsetor_cnae_fust = collapse_unique(subsetor_cnae_agrupado),
    porte_fust = collapse_unique(porte_do_cliente)
  ),
  by = .(cliente_norm, cnpj_original = cnpj)
]
entity_unresolved[is.infinite(ano_primeira_operacao_fust), ano_primeira_operacao_fust := NA_integer_]
entity_unresolved[is.infinite(ano_ultima_operacao_fust), ano_ultima_operacao_fust := NA_integer_]
setorder(entity_unresolved, cliente_norm, cnpj_original)
entity_unresolved[, id_empresa_painel := paste0("SEM_CNPJ_RAIZ_", sprintf("%04d", .I))]
entity_unresolved[, cnpj_raiz := NA_character_]
entity_unresolved[, c("cliente_norm", "cnpj_original") := NULL]

setcolorder(entity_unresolved, names(entity_found))
entity_ref <- rbindlist(list(entity_found, entity_unresolved), fill = TRUE, use.names = TRUE)

rais_company_ref <- estab_resolve[
  cnpj_raiz %in% entity_ref$cnpj_raiz,
  {
    ordered <- order(cnpj_ordem != "0001", cnpj)
    .(
      cnpj_matriz_rais = cnpj[ordered][1],
      razao_social_rais = razao_social[ordered][1]
    )
  },
  by = cnpj_raiz
]
entity_ref <- merge(entity_ref, rais_company_ref, by = "cnpj_raiz", all.x = TRUE)

panel <- entity_ref[rep(seq_len(nrow(entity_ref)), each = length(years))]
panel[, ano := rep(years, times = nrow(entity_ref))]

vinc_company <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      massa_salarial_media = sum(massa_salarial_media, na.rm = TRUE),
      massa_salarial_dezembro = sum(massa_salarial_dezembro, na.rm = TRUE),
      novos_empregos = sum(novos_empregos, na.rm = TRUE),
      desligamentos = sum(desligamentos, na.rm = TRUE),
      saldo_empregos = sum(saldo_empregos, na.rm = TRUE),
      vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz)
  ]
} else {
  data.table()
}
if (nrow(vinc_company) > 0) {
  vinc_company[
    ,
    rendimento_medio := fifelse(
      total_empregados > 0,
      massa_salarial_media / total_empregados,
      NA_real_
    )
  ]
}

estab_company <- if (nrow(estab_metrics) > 0) {
  estab_metrics[
    ,
    .(
      total_firmas = sum(total_firmas, na.rm = TRUE),
      total_empregados_estab = sum(total_empregados_estab, na.rm = TRUE),
      total_empregados_clt_estab = sum(total_empregados_clt_estab, na.rm = TRUE),
      total_empregados_estatutarios_estab = sum(total_empregados_estatutarios_estab, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz)
  ]
} else {
  data.table()
}

presence_estab <- unique(estab_metrics[, .(ano, cnpj_raiz)])
presence_estab[, encontrado_rais_estabelecimentos := 1L]
presence_vinc <- unique(vinc_metrics[, .(ano, cnpj_raiz)])
presence_vinc[, encontrado_rais_vinculos := 1L]

uf_agg <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    !is.na(uf),
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, uf)
  ]
} else {
  data.table()
}

uf_main <- if (nrow(uf_agg) > 0) {
  temp <- copy(uf_agg)
  setorder(temp, ano, cnpj_raiz, -total_empregados, -vinculos_rais, uf)
  temp[
    ,
    .(
      uf_principal = first(uf),
      empregados_uf_principal = first(total_empregados),
      numero_ufs_com_empregados = uniqueN(uf[total_empregados > 0])
    ),
    by = .(ano, cnpj_raiz)
  ]
} else {
  data.table()
}

all_ufs <- c(
  "AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO",
  "MA", "MG", "MS", "MT", "PA", "PB", "PE", "PI", "PR",
  "RJ", "RN", "RO", "RR", "RS", "SC", "SE", "SP", "TO"
)

uf_wide <- if (nrow(uf_agg) > 0) {
  dcast(
    uf_agg,
    ano + cnpj_raiz ~ uf,
    value.var = "total_empregados",
    fun.aggregate = sum,
    fill = 0
  )
} else {
  data.table(ano = integer(), cnpj_raiz = character())
}
for (uf_name in setdiff(all_ufs, names(uf_wide))) {
  uf_wide[, (uf_name) := 0]
}
setcolorder(uf_wide, c("ano", "cnpj_raiz", all_ufs))
setnames(uf_wide, all_ufs, paste0("empregados_uf_", all_ufs))

sector_agg <- if (nrow(vinc_metrics) > 0) {
  vinc_metrics[
    ,
    .(
      total_empregados = sum(total_empregados, na.rm = TRUE),
      vinculos_rais = sum(vinculos_rais, na.rm = TRUE)
    ),
    by = .(ano, cnpj_raiz, setor_cnae_classe, setor_cnae_subclasse, ibge_subsetor)
  ]
} else {
  data.table()
}
if (nrow(sector_agg) > 0) {
  sector_agg[
    ,
    subsetor_ibge_codigo := fifelse(
      is.na(parse_number_br(ibge_subsetor)),
      NA_character_,
      sprintf("%02d", as.integer(parse_number_br(ibge_subsetor)))
    )
  ]
}

sector_main <- if (nrow(sector_agg) > 0) {
  temp <- copy(sector_agg)
  setorder(
    temp,
    ano,
    cnpj_raiz,
    -total_empregados,
    -vinculos_rais,
    subsetor_ibge_codigo,
    setor_cnae_subclasse
  )
  temp[
    ,
    .(
      setor_cnae_classe_principal = first(setor_cnae_classe),
      setor_cnae_subclasse_principal = first(setor_cnae_subclasse),
      subsetor_ibge_principal_codigo = first(subsetor_ibge_codigo),
      empregados_setor_principal = first(total_empregados),
      numero_setores_com_empregados = uniqueN(subsetor_ibge_codigo[
        total_empregados > 0 & !is.na(subsetor_ibge_codigo)
      ])
    ),
    by = .(ano, cnpj_raiz)
  ]
} else {
  data.table()
}
if (nrow(sector_main) > 0) {
  sector_main[
    ,
    subsetor_ibge_principal_nome := subsetor_ibge_label(subsetor_ibge_principal_codigo)
  ]
}

all_subsetores <- sprintf("%02d", 1:25)
sector_wide_source <- sector_agg[
  !is.na(subsetor_ibge_codigo),
  .(total_empregados = sum(total_empregados, na.rm = TRUE)),
  by = .(ano, cnpj_raiz, subsetor_ibge_codigo)
]
sector_wide <- if (nrow(sector_wide_source) > 0) {
  dcast(
    sector_wide_source,
    ano + cnpj_raiz ~ subsetor_ibge_codigo,
    value.var = "total_empregados",
    fun.aggregate = sum,
    fill = 0
  )
} else {
  data.table(ano = integer(), cnpj_raiz = character())
}
for (sector_code in setdiff(all_subsetores, names(sector_wide))) {
  sector_wide[, (sector_code) := 0]
}
setcolorder(sector_wide, c("ano", "cnpj_raiz", all_subsetores))
setnames(
  sector_wide,
  all_subsetores,
  paste0("empregados_subsetor_ibge_", all_subsetores)
)

panel <- merge(panel, presence_estab, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, presence_vinc, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, vinc_company, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, estab_company, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, uf_main, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, sector_main, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, uf_wide, by = c("ano", "cnpj_raiz"), all.x = TRUE)
panel <- merge(panel, sector_wide, by = c("ano", "cnpj_raiz"), all.x = TRUE)

panel[
  ,
  encontrado_rais_estabelecimentos := fifelse(
    is.na(encontrado_rais_estabelecimentos),
    0L,
    encontrado_rais_estabelecimentos
  )
]
panel[
  ,
  encontrado_rais_vinculos := fifelse(
    is.na(encontrado_rais_vinculos),
    0L,
    encontrado_rais_vinculos
  )
]
panel[
  ,
  encontrado_rais := as.integer(
    encontrado_rais_estabelecimentos == 1L | encontrado_rais_vinculos == 1L
  )
]

employment_zero_columns <- c(
  "total_empregados",
  "massa_salarial_media",
  "massa_salarial_dezembro",
  "novos_empregos",
  "desligamentos",
  "saldo_empregos",
  "vinculos_rais",
  "empregados_uf_principal",
  "numero_ufs_com_empregados",
  "empregados_setor_principal",
  "numero_setores_com_empregados",
  paste0("empregados_uf_", all_ufs),
  paste0("empregados_subsetor_ibge_", all_subsetores)
)
for (column in employment_zero_columns) {
  set(
    panel,
    i = which(panel$encontrado_rais == 1L & is.na(panel[[column]])),
    j = column,
    value = 0
  )
}

estab_zero_columns <- c(
  "total_firmas",
  "total_empregados_estab",
  "total_empregados_clt_estab",
  "total_empregados_estatutarios_estab"
)
for (column in estab_zero_columns) {
  set(
    panel,
    i = which(panel$encontrado_rais_estabelecimentos == 1L & is.na(panel[[column]])),
    j = column,
    value = 0
  )
}

panel[
  encontrado_rais == 1L & !is.na(total_empregados),
  ranking_empregos_ano := frank(-total_empregados, ties.method = "dense"),
  by = ano
]

setorder(panel, id_empresa_painel, ano)
panel[
  ,
  variacao_empregados := total_empregados - shift(total_empregados),
  by = id_empresa_painel
]
panel[
  ,
  taxa_crescimento_empregados := fifelse(
    !is.na(shift(total_empregados)) & shift(total_empregados) > 0,
    variacao_empregados / shift(total_empregados),
    NA_real_
  ),
  by = id_empresa_painel
]

setcolorder(
  panel,
  c(
    "id_empresa_painel",
    "ano",
    "cnpj_raiz",
    "cnpj_matriz_rais",
    "cnpj_fust",
    "cliente_fust",
    "razao_social_rais",
    "status_chave",
    "encontrado_rais",
    "encontrado_rais_estabelecimentos",
    "encontrado_rais_vinculos",
    "ranking_empregos_ano",
    "total_empregados",
    "total_firmas",
    "massa_salarial_media",
    "massa_salarial_dezembro",
    "rendimento_medio",
    "novos_empregos",
    "desligamentos",
    "saldo_empregos",
    "variacao_empregados",
    "taxa_crescimento_empregados",
    "uf_principal",
    "empregados_uf_principal",
    "numero_ufs_com_empregados",
    "setor_cnae_classe_principal",
    "setor_cnae_subclasse_principal",
    "subsetor_ibge_principal_codigo",
    "subsetor_ibge_principal_nome",
    "empregados_setor_principal",
    "numero_setores_com_empregados",
    "qtd_operacoes_fust",
    "valor_operacao_fust",
    "valor_desembolsado_fust",
    "ano_primeira_operacao_fust",
    "ano_ultima_operacao_fust",
    "uf_fust",
    "municipio_fust",
    "setor_cnae_fust",
    "subsetor_cnae_fust",
    "porte_fust",
    "total_empregados_estab",
    "total_empregados_clt_estab",
    "total_empregados_estatutarios_estab",
    "vinculos_rais",
    paste0("empregados_uf_", all_ufs),
    paste0("empregados_subsetor_ibge_", all_subsetores)
  )
)

dictionary <- data.table(
  coluna = c(
    "id_empresa_painel",
    "ano",
    "cnpj_raiz",
    "cnpj_matriz_rais",
    "cnpj_fust",
    "encontrado_rais",
    "ranking_empregos_ano",
    "total_empregados",
    "total_firmas",
    "massa_salarial_media",
    "massa_salarial_dezembro",
    "rendimento_medio",
    "novos_empregos",
    "desligamentos",
    "saldo_empregos",
    "variacao_empregados",
    "taxa_crescimento_empregados",
    "uf_principal",
    "setor_cnae_subclasse_principal",
    "empregados_uf_XX",
    "empregados_subsetor_ibge_XX"
  ),
  descricao = c(
    "Identificador balanceado da empresa no painel.",
    "Ano de referencia da RAIS.",
    "Oito primeiros digitos do CNPJ, usados para agregar matriz e filiais.",
    "CNPJ completo da matriz encontrado na RAIS, quando disponivel.",
    "CNPJ original, completo ou mascarado, presente na base FUST.",
    "1 quando o CNPJ raiz aparece na RAIS do ano; 0 quando as metricas RAIS devem ficar missing.",
    "Posicao da empresa no total de empregados dentro do ano.",
    "Vinculos ativos em 31/12.",
    "Numero de estabelecimentos distintos do CNPJ raiz.",
    "Soma da remuneracao media nominal dos vinculos ativos.",
    "Soma da remuneracao nominal de dezembro dos vinculos ativos.",
    "Massa salarial media dividida pelo total de empregados ativos.",
    "Vinculos cuja data de admissao pertence ao ano.",
    "Vinculos com mes ou motivo de desligamento informado no ano.",
    "Novos empregos menos desligamentos.",
    "Total de empregados menos o valor do ano anterior.",
    "Variacao de empregados dividida pelo total do ano anterior.",
    "UF com maior numero de empregados ativos.",
    "CNAE subclasse com maior numero de empregados ativos.",
    "Empregados ativos na UF XX. Existem 27 colunas, uma por UF.",
    "Empregados ativos no subsetor IBGE XX. Existem 25 colunas."
  )
)

expected_rows <- uniqueN(panel$id_empresa_painel) * length(years)
if (nrow(panel) != expected_rows) {
  stop(
    "Painel desbalanceado: esperado ",
    expected_rows,
    " linhas, mas foram geradas ",
    nrow(panel),
    ".",
    call. = FALSE
  )
}

if (anyDuplicated(panel[, .(id_empresa_painel, ano)]) > 0) {
  stop("Painel contem duplicatas de empresa-ano.", call. = FALSE)
}

company_year_counts <- panel[, .N, by = id_empresa_painel]
if (any(company_year_counts$N != length(years))) {
  stop("Ao menos uma empresa nao possui todos os anos do painel.", call. = FALSE)
}

rais_metric_columns <- c(
  "ranking_empregos_ano",
  "total_empregados",
  "total_firmas",
  "massa_salarial_media",
  "massa_salarial_dezembro",
  "rendimento_medio",
  "novos_empregos",
  "desligamentos",
  "saldo_empregos",
  "variacao_empregados",
  "taxa_crescimento_empregados",
  "uf_principal",
  "empregados_uf_principal",
  "numero_ufs_com_empregados",
  "setor_cnae_classe_principal",
  "setor_cnae_subclasse_principal",
  "subsetor_ibge_principal_codigo",
  "subsetor_ibge_principal_nome",
  "empregados_setor_principal",
  "numero_setores_com_empregados",
  "total_empregados_estab",
  "total_empregados_clt_estab",
  "total_empregados_estatutarios_estab",
  "vinculos_rais",
  paste0("empregados_uf_", all_ufs),
  paste0("empregados_subsetor_ibge_", all_subsetores)
)

invalid_missing_rows <- panel[
  encontrado_rais == 0L,
  sum(rowSums(!is.na(.SD)) > 0),
  .SDcols = rais_metric_columns
]
if (invalid_missing_rows > 0) {
  stop(
    invalid_missing_rows,
    " linhas sem correspondencia RAIS possuem metricas RAIS preenchidas.",
    call. = FALSE
  )
}

writexl::write_xlsx(
  list(
    painel_cnpj_ano = panel,
    dicionario = dictionary
  ),
  panel_file
)

message("Painel balanceado gerado: ", panel_file)
if (!only_panel_xlsx) {
  panel_csv <- str_replace(panel_file, regex("\\.xlsx$", ignore_case = TRUE), ".csv")
  fwrite(panel, panel_csv, bom = TRUE)
  message("Painel CSV gerado: ", panel_csv)
}
message("Empresas no painel: ", uniqueN(panel$id_empresa_painel))
message("Linhas no painel: ", nrow(panel))

fust_keys_out <- fust_keys[
  ,
  .(
    operacao_id,
    cliente,
    cnpj_original = cnpj,
    cnpj_parcial = cnpj_digits,
    cnpj_raiz,
    status_chave,
    cnpjs_raiz_encontrados,
    cnpjs_raiz_candidatos,
    melhor_score_nome,
    valor_operacao_r,
    valor_desembolsado_r,
    uf,
    municipio,
    setor_cnae,
    subsetor_cnae_agrupado,
    porte_do_cliente
  )
]

unresolved <- fust_keys_out[is.na(cnpj_raiz)]

metadata <- data.table(
  item = c(
    "anos",
    "arquivo_fust",
    "dir_rais",
    "dir_rais2",
    "operacoes_fust",
    "operacoes_com_chave",
    "operacoes_sem_chave",
    "cnpj_raiz_cruzados",
    "ano_resolucao_nome"
  ),
  valor = c(
    paste(years, collapse = ", "),
    fust_file,
    rais_dir,
    rais2_dir,
    nrow(fust),
    sum(!is.na(fust_keys_out$cnpj_raiz)),
    nrow(unresolved),
    length(target_roots),
    resolve_year
  )
)

if (!only_panel_xlsx) {
  tables <- list(
    metadados = metadata,
    fust_chaves_cnpj = fust_keys_out,
    fust_sem_chave = unresolved,
    rais_empresas_ranking = ranking_empresas,
    rais_resumo_empresas = resumo_empresas,
    rais_emprego_setor = emprego_setor,
    rais_emprego_uf = emprego_uf,
    rais_estabelecimentos_uf_setor = estab_metrics,
    rais_matches_nome = name_matches,
    rais_matches_cnpj_mascarado = masked_matches
  )

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  xlsx_out <- file.path(
    output_dir,
    paste0("RAIS_FUST_", paste(years, collapse = "-"), "_", timestamp, ".xlsx")
  )
  writexl::write_xlsx(tables, xlsx_out)

  for (name in names(tables)) {
    fwrite(tables[[name]], file.path(output_dir, paste0(name, ".csv")), bom = TRUE)
  }

  message("Arquivo Excel gerado: ", xlsx_out)
  message("CSVs gerados em: ", output_dir)
}
