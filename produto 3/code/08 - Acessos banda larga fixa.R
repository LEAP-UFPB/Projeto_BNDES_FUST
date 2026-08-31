#!/usr/bin/env Rscript

# Efeitos do FUST sobre o estoque de acessos de banda larga fixa da Anatel.
#
# Duas especificacoes sao estimadas, mantendo a definicao de tratamento e o
# estimador de Callaway--Sant'Anna usados nas figuras principais da RAIS:
#   1. todas as prestadoras nunca tratadas observadas na base da Anatel;
#   2. matching 1:1 escolhido por balanceamento de variaveis pre-FUST da RAIS.
# O workbook preserva ainda as alternativas RAIS-only, sem selecionar controles
# pelos acessos observados na Anatel.
#
# A unidade e a raiz do CNPJ. O outcome e o log(1 + acessos) no mes de
# dezembro, agregado sobre estabelecimentos, municipios, tecnologias, faixas
# de velocidade, tipos de pessoa e produtos. A soma dos meses seria incorreta,
# pois acessos e uma medida de estoque.

suppressPackageStartupMessages({
  library(data.table)
  library(did)
  library(ggplot2)
  library(MatchIt)
  library(openxlsx)
})

set.seed(3854035)

args <- commandArgs(FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) stop("Execute este arquivo com Rscript.")
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

tipo_produto <- toupper(trimws(Sys.getenv("ANATEL_TIPO_PRODUTO", "TODOS")))
if (!tipo_produto %chin% c("TODOS", "INTERNET")) {
  stop("ANATEL_TIPO_PRODUTO deve ser TODOS ou INTERNET.")
}
internet_only <- identical(tipo_produto, "INTERNET")

# A classificacao por Tipo de Produto so esta disponivel a partir de 2021.
# Assim, o recorte estrito de internet nao imputa retrospectivamente a
# classificacao ausente em 2020.
years <- if (internet_only) 2021:2025 else 2020:2025
matching_cohorts <- c(2024L, 2025L)
output_suffix <- if (internet_only) " - Somente internet" else ""
cache_file <- file.path(
  data_dir,
  if (internet_only) "produto3_painel_banda_larga_fixa_internet.csv" else
    "produto3_painel_banda_larga_fixa.csv"
)
results_file <- file.path(
  output_dir, paste0("Resultados - Acessos banda larga fixa", output_suffix, ".xlsx")
)
figure_file <- file.path(
  output_dir,
  paste0("Event study - CS - Log-acessos-banda-larga", output_suffix, ".png")
)
modality_figure_file <- file.path(
  output_dir,
  paste0(
    "Event study - CS - Log-acessos-banda-larga - Direto e indireto",
    output_suffix, ".png"
  )
)

clean_root <- function(x) {
  z <- gsub("\\D", "", as.character(x))
  short <- !is.na(z) & nchar(z) < 8L
  z[short] <- sprintf("%08d", suppressWarnings(as.integer(z[short])))
  substr(z, 1L, 8L)
}

source_files <- c(
  `2020` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2019_2020_Colunas.csv"),
  `2021` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2021_Colunas.csv"),
  `2022` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2022_Colunas.csv"),
  `2023` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2023_Colunas.csv"),
  `2024` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2024_Colunas.csv"),
  `2025` = file.path(data_dir, "Acessos_Banda_Larga_Fixa_2025_Colunas.csv")
)
missing_files <- source_files[!file.exists(source_files)]
if (length(missing_files)) {
  stop("Arquivos da Anatel nao encontrados: ", paste(basename(missing_files), collapse = ", "))
}

# O cache e pequeno e torna as reexecucoes independentes dos CSVs de 100--160 MB.
# Ele so e reutilizado quando contem toda a janela requerida e as colunas de
# proveniencia/auditoria criadas por este script.
cache_ok <- FALSE
if (file.exists(cache_file)) {
  cached <- fread(cache_file, sep = ";", colClasses = c(cnpj_raiz = "character"))
  cache_ok <- all(c("cnpj_raiz", "ano", "acessos", "observada_no_ano",
                    "log_acessos") %in% names(cached)) &&
    identical(sort(unique(cached$ano)), years)
}

if (cache_ok) {
  panel <- cached
  panel[, cnpj_raiz := clean_root(cnpj_raiz)]
  message("Painel Anatel carregado do cache: ", basename(cache_file))
} else {
  annual <- vector("list", length(years))
  names(annual) <- as.character(years)

  for (yr in years) {
    f <- source_files[[as.character(yr)]]
    header <- names(fread(f, nrows = 0L, encoding = "UTF-8"))
    month_cols <- grep(paste0("^", yr, "-[0-9]{2}$"), header, value = TRUE)
    december <- paste0(yr, "-12")
    required_columns <- c("CNPJ", december)
    if (internet_only) required_columns <- c(required_columns, "Tipo de Produto")
    if (!all(required_columns %in% header)) {
      stop(
        "Colunas obrigatorias ausentes em ", basename(f), ": ",
        paste(setdiff(required_columns, header), collapse = ", ")
      )
    }

    raw <- fread(
      f, select = c("CNPJ", if (internet_only) "Tipo de Produto", month_cols),
      colClasses = list(character = "CNPJ"), encoding = "UTF-8",
      showProgress = FALSE
    )
    if (internet_only) {
      raw <- raw[toupper(trimws(`Tipo de Produto`)) == "INTERNET"]
    }
    raw[, cnpj_raiz := clean_root(CNPJ)]
    raw <- raw[grepl("^[0-9]{8}$", cnpj_raiz)]

    by_root <- raw[, lapply(.SD, sum, na.rm = TRUE),
                   by = cnpj_raiz, .SDcols = month_cols]
    by_root[, observada_no_ano := rowSums(as.matrix(.SD), na.rm = TRUE) > 0,
            .SDcols = month_cols]
    by_root[, `:=`(
      ano = yr,
      acessos = as.numeric(get(december))
    )]
    annual[[as.character(yr)]] <- by_root[, .(
      cnpj_raiz, ano, acessos, observada_no_ano
    )]
    message(sprintf(
      "Anatel %d (%s): %s raizes ativas no ano; %s com acessos em dezembro",
      yr,
      if (internet_only) "INTERNET" else "todos os produtos",
      format(by_root[observada_no_ano == TRUE, .N], big.mark = "."),
      format(by_root[get(december) > 0, .N], big.mark = ".")
    ))
    rm(raw, by_root)
    gc(FALSE)
  }

  extracted <- rbindlist(annual)
  observed_roots <- unique(extracted[observada_no_ano == TRUE, cnpj_raiz])
  panel <- merge(
    CJ(cnpj_raiz = observed_roots, ano = years),
    extracted, by = c("cnpj_raiz", "ano"), all.x = TRUE
  )
  panel[is.na(acessos), acessos := 0]
  panel[is.na(observada_no_ano), observada_no_ano := FALSE]
  panel[, log_acessos := log1p(acessos)]
  setorder(panel, cnpj_raiz, ano)
  fwrite(panel, cache_file, sep = ";", bom = TRUE)
  message("Painel Anatel salvo em ", cache_file)
}

if (any(panel$acessos < 0, na.rm = TRUE)) stop("O painel contem acessos negativos.")
if (panel[, anyDuplicated(paste(cnpj_raiz, ano))]) stop("CNPJ-ano duplicado no painel.")
observed_roots <- unique(panel[observada_no_ano == TRUE, cnpj_raiz])

# Tratadas e pares oficiais definidos pela especificacao S4 do Produto 3.
units_file <- file.path(data_dir, "produto3_unidades_matching.csv")
official <- fread(units_file, sep = ";", colClasses = c(cnpj_raiz = "character"))
needed <- c("cnpj_raiz", "treat", "coorte_match", "subclass")
if (!all(needed %in% names(official))) {
  stop("A amostra oficial da RAIS nao contem: ",
       paste(setdiff(needed, names(official)), collapse = ", "))
}
official[, cnpj_raiz := clean_root(cnpj_raiz)]
official <- unique(official[, ..needed])
official[, pair_id := paste(coorte_match, subclass, sep = "|")]
pair_check <- official[, .(
  n = .N, n_treated = sum(treat == 1L), n_control = sum(treat == 0L)
), by = pair_id]
if (pair_check[, any(n != 2L | n_treated != 1L | n_control != 1L)]) {
  stop("A amostra oficial nao possui estrutura 1:1 valida.")
}

# Exclui do universo de controles qualquer recebedora do FUST, inclusive as
# que nao permaneceram na amostra S4. Isso evita controles tratados ocultos.
covars <- fread(
  file.path(data_dir, "produto3_covariaveis_matching.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
covars[, cnpj_raiz := clean_root(cnpj_raiz)]
covars[, division := substr(as.character(as.integer(cnae_classe_principal)), 1L, 2L)]
all_fust_roots <- unique(covars[treat == 1L, cnpj_raiz])

official_treated <- unique(official[treat == 1L, .(cnpj_raiz, g = as.integer(coorte_match))])
treated_anatel <- official_treated[cnpj_raiz %chin% observed_roots]
all_control_roots <- setdiff(observed_roots, all_fust_roots)

# Modalidade do primeiro apoio. A classificacao foi produzida na montagem do
# painel FUST--RAIS; empresas com apoios nas duas modalidades sao classificadas
# pela modalidade da operacao que define sua primeira coorte de tratamento.
modality <- fread(
  file.path(data_dir, "produto3_modalidade_fust.csv"), sep = ";",
  colClasses = c(cnpj_raiz = "character")
)
modality[, cnpj_raiz := clean_root(cnpj_raiz)]
modality <- unique(modality[
  grepl("^[0-9]{8}$", cnpj_raiz), .(cnpj_raiz, modalidade = tipo_operacao)
])
if (modality[, anyDuplicated(cnpj_raiz)]) {
  stop("Ha CNPJs com mais de uma classificacao de modalidade.")
}
treated_modality <- merge(treated_anatel, modality, by = "cnpj_raiz", all.x = TRUE)
if (treated_modality[, any(is.na(modalidade))]) {
  stop("Ha tratadas observadas na Anatel sem modalidade de apoio.")
}

units_all <- rbindlist(list(
  treated_anatel,
  data.table(cnpj_raiz = all_control_roots, g = 0L)
))
units_all <- unique(units_all, by = "cnpj_raiz")
units_all[, g := as.numeric(g)]

# Refaz o matching 1:1 depois de restringir a amostra as prestadoras presentes
# na Anatel. O desenho usa exclusivamente emprego e salario medio da RAIS no
# ano anterior ao apoio. Ausencia de vinculo ativo equivale a emprego e salario
# medio zero. Nao ha exact, caliper ou variavel da Anatel no pareamento.
rais_matching_file <- file.path(
  data_dir, "produto3_covariaveis_rais_matching_banda_larga.csv"
)
if (!file.exists(rais_matching_file)) {
  stop(
    "Cache RAIS ausente. Execute primeiro: ",
    file.path(dirname(script), "_extract_rais_matching_banda_larga.R")
  )
}
rais_matching <- fread(
  rais_matching_file, sep = ";", colClasses = c(cnpj_raiz = "character")
)
rais_matching[, cnpj_raiz := clean_root(cnpj_raiz)]
employment_history_file <- file.path(
  data_dir, "produto3_emprego_rais_matching_banda_larga.csv"
)
if (!file.exists(employment_history_file)) {
  stop(
    "Historico de emprego RAIS ausente. Execute primeiro: ",
    file.path(dirname(script), "_extract_rais_matching_banda_larga.R")
  )
}
employment_history <- fread(
  employment_history_file, sep = ";", colClasses = c(cnpj_raiz = "character")
)
employment_history[, cnpj_raiz := clean_root(cnpj_raiz)]

common_balance_vars <- c(
  "log_emprego_t3", "log_emprego_t2", "log_emprego_t1",
  "log_salario_medio_t1"
)
rich_rais_vars <- c(
  "idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores",
  "idade_media_trabalhadores", "prop_homens", "prop_pretos"
)

make_matching_pool <- function(
    g0, treated_ids, used_controls,
    restrict_rais_pool = FALSE, include_rich = FALSE,
    include_categories = FALSE) {
  eligible_roots <- c(treated_ids, setdiff(all_control_roots, used_controls))
  pool <- data.table(cnpj_raiz = unique(eligible_roots))

  h <- employment_history[ano %in% (g0 - 3L):(g0 - 1L), .(
    cnpj_raiz, lag = g0 - ano, log_emprego = log1p(emprego)
  )]
  h[, variavel := paste0("log_emprego_t", lag)]
  h <- dcast(h, cnpj_raiz ~ variavel, value.var = "log_emprego", fill = 0)
  salary <- rais_matching[ano == g0 - 1L, .(
    cnpj_raiz, log_salario_medio_t1 = log1p(fcoalesce(salario_medio, 0))
  )]
  pool <- merge(pool, h, by = "cnpj_raiz", all.x = TRUE)
  pool <- merge(pool, salary, by = "cnpj_raiz", all.x = TRUE)
  for (v in common_balance_vars) set(pool, which(is.na(pool[[v]])), v, 0)
  if (restrict_rais_pool || include_rich || include_categories) {
    rais_columns <- c("cnpj_raiz", rich_rais_vars)
    if (include_categories) {
      rais_columns <- c(
        rais_columns, "uf_principal", "division", "tamanho_estab_principal"
      )
    }
    rich <- unique(
      covars[g == g0, ..rais_columns],
      by = "cnpj_raiz"
    )
    pool <- merge(pool, rich, by = "cnpj_raiz", all = FALSE)
  }
  if (include_rich) {
    pool <- pool[complete.cases(pool[, ..rich_rais_vars])]
  }
  pool[, treat_match := as.integer(cnpj_raiz %chin% treated_ids)]
  if (!all(treated_ids %chin% pool$cnpj_raiz)) {
    stop("A restricao RAIS eliminou tratadas da coorte ", g0)
  }
  pool
}

smd_value <- function(m, v) {
  a <- m[treat_match == 1L, get(v)]
  b <- m[treat_match == 0L, get(v)]
  den <- sqrt((var(a) + var(b)) / 2)
  ifelse(is.finite(den) && den > 0, (mean(a) - mean(b)) / den, 0)
}

matching_designs <- list(
  nivel_mahalanobis = list(
    label = "1:1 RAIS: nivel em g-1 (Mahalanobis)",
    vars = c("log_emprego_t1", "log_salario_medio_t1"),
    distance = "mahalanobis",
    orders = c("farthest", "closest")
  ),
  trajetoria_mahalanobis = list(
    label = "1:1 RAIS: trajetoria pre (Mahalanobis)",
    vars = common_balance_vars,
    distance = "mahalanobis",
    orders = c("farthest", "closest")
  ),
  trajetoria_propensity = list(
    label = "1:1 RAIS: trajetoria pre (propensity score)",
    vars = common_balance_vars,
    distance = "glm",
    orders = c("largest", "smallest", "closest", "farthest"),
    restrict_rais_pool = FALSE, include_rich = FALSE
  ),
  raispool_nivel_mahalanobis = list(
    label = "1:1 universo RAIS: nivel em g-1 (Mahalanobis)",
    vars = c("log_emprego_t1", "log_salario_medio_t1"),
    distance = "mahalanobis", orders = c("farthest", "closest"),
    restrict_rais_pool = TRUE, include_rich = FALSE
  ),
  raispool_ampliado_mahalanobis = list(
    label = "1:1 universo RAIS: covariadas ampliadas (Mahalanobis)",
    vars = c(common_balance_vars, rich_rais_vars),
    distance = "mahalanobis", orders = c("farthest", "closest"),
    restrict_rais_pool = TRUE, include_rich = TRUE
  ),
  raispool_ampliado_propensity = list(
    label = "1:1 universo RAIS: covariadas ampliadas (propensity score)",
    vars = c(common_balance_vars, rich_rais_vars),
    distance = "glm", orders = c("largest", "smallest", "closest", "farthest"),
    restrict_rais_pool = TRUE, include_rich = TRUE
  ),
  raispool_perfil_setor_uf = list(
    label = "1:1 universo RAIS: perfil, setor e UF (propensity score)",
    vars = c(common_balance_vars, rich_rais_vars),
    formula_terms = c(
      common_balance_vars, rich_rais_vars,
      "factor(uf_principal)", "factor(division)",
      "factor(tamanho_estab_principal)"
    ),
    categorical_vars = c("uf_principal", "division", "tamanho_estab_principal"),
    distance = "glm", orders = c("largest", "smallest", "closest", "farthest"),
    restrict_rais_pool = TRUE, include_rich = TRUE, include_categories = TRUE
  )
)

for (nm in names(matching_designs)) {
  if (is.null(matching_designs[[nm]]$restrict_rais_pool)) {
    matching_designs[[nm]]$restrict_rais_pool <- FALSE
  }
  if (is.null(matching_designs[[nm]]$include_rich)) {
    matching_designs[[nm]]$include_rich <- FALSE
  }
  if (is.null(matching_designs[[nm]]$include_categories)) {
    matching_designs[[nm]]$include_categories <- FALSE
  }
  if (is.null(matching_designs[[nm]]$formula_terms)) {
    matching_designs[[nm]]$formula_terms <- matching_designs[[nm]]$vars
  }
  if (is.null(matching_designs[[nm]]$categorical_vars)) {
    matching_designs[[nm]]$categorical_vars <- character()
  }
}

run_matching_design <- function(design_name, design) {
  used_controls <- character()
  matched_parts <- list()
  balance_parts <- list()
  choice_parts <- list()

  for (g0 in matching_cohorts) {
    treated_ids <- treated_anatel[g == g0, cnpj_raiz]
    pool <- make_matching_pool(
      g0, treated_ids, used_controls,
      restrict_rais_pool = design$restrict_rais_pool,
      include_rich = design$include_rich,
      include_categories = design$include_categories
    )
    candidate_specs <- rbindlist(list(
      data.table(order = design$orders, seed = 0L),
      data.table(order = "random", seed = 1:50)
    ))
    candidates <- lapply(seq_len(nrow(candidate_specs)), function(i) {
      z <- candidate_specs[i]
      set.seed(3854035 + z$seed)
      args_match <- list(
        formula = reformulate(design$formula_terms, "treat_match"), data = pool,
        method = "nearest", distance = design$distance,
        ratio = 1, replace = FALSE, m.order = z$order
      )
      if (identical(design$distance, "glm")) args_match$link <- "logit"
      fit <- tryCatch(do.call(matchit, args_match), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      m <- as.data.table(match.data(fit, drop.unmatched = TRUE))
      smds <- vapply(design$vars, function(v) smd_value(m, v), numeric(1))
      category_differences <- unlist(lapply(design$categorical_vars, function(v) {
        levels_v <- union(
          unique(m[treat_match == 1L, as.character(get(v))]),
          unique(m[treat_match == 0L, as.character(get(v))])
        )
        vapply(levels_v, function(lev) {
          mean(m[treat_match == 1L, as.character(get(v)) == lev]) -
            mean(m[treat_match == 0L, as.character(get(v)) == lev])
        }, numeric(1))
      }))
      score <- max(abs(c(smds, category_differences)))
      list(m = m, score = score, order = z$order, seed = z$seed)
    })
    candidates <- Filter(Negate(is.null), candidates)
    if (!length(candidates)) stop("Nenhum matching valido para ", design_name, "/", g0)
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "score"))]]
    matched_g <- best$m
    matched_g[, coorte_match := g0]
    if (matched_g[treat_match == 1L, .N] != length(treated_ids) ||
        matched_g[treat_match == 0L, .N] != length(treated_ids)) {
      stop("Matching 1:1 incompleto em ", design_name, "/", g0)
    }

    balance_vars <- unique(c(common_balance_vars, intersect(rich_rais_vars, names(pool))))
    balance_parts[[as.character(g0)]] <- rbindlist(lapply(
      balance_vars, function(v) data.table(
        desenho = design_name, descricao = design$label, coorte = g0,
        variavel = v,
        media_tratadas = mean(matched_g[treat_match == 1L, get(v)]),
        media_controles = mean(matched_g[treat_match == 0L, get(v)]),
        smd = smd_value(matched_g, v),
        usada_no_matching = v %in% design$vars
      )
    ))
    choice_parts[[as.character(g0)]] <- data.table(
      desenho = design_name, descricao = design$label, coorte = g0,
      ordem = best$order, seed = best$seed,
      criterio_balanceamento = best$score
    )
    matched_parts[[as.character(g0)]] <- matched_g
    used_controls <- union(used_controls, matched_g[treat_match == 0L, cnpj_raiz])
    message(sprintf(
      "%s / %d: %d pares; criterio |SMD| = %.3f; ordem = %s; seed = %d",
      design_name, g0, length(treated_ids), best$score, best$order, best$seed
    ))
  }
  list(
    parts = matched_parts,
    balance = rbindlist(balance_parts),
    choices = rbindlist(choice_parts)
  )
}

matching_results <- Map(
  run_matching_design, names(matching_designs), matching_designs
)
matching_balance <- rbindlist(lapply(matching_results, `[[`, "balance"))
matching_choices <- rbindlist(lapply(matching_results, `[[`, "choices"))
matching_ranking <- matching_balance[, .(
  maior_abs_smd = max(abs(smd)),
  media_abs_smd = mean(abs(smd))
), by = .(desenho, descricao)]
primary_candidates <- c(
  "nivel_mahalanobis", "trajetoria_mahalanobis", "trajetoria_propensity"
)
setorder(matching_ranking, maior_abs_smd, media_abs_smd)
preferred_design <- matching_ranking[desenho %chin% primary_candidates, desenho][1L]
matching_ranking[, selecionado_balanceamento_rais := desenho == preferred_design]

matched_parts <- matching_results[[preferred_design]]$parts
matched <- rbindlist(matched_parts, fill = TRUE)
units_matched <- matched[, .(
  g = ifelse(treat_match == 1L, as.numeric(coorte_match), 0)
), by = cnpj_raiz]
matching_file <- file.path(
  data_dir,
  if (internet_only) "produto3_unidades_matching_banda_larga_internet.csv" else
    "produto3_unidades_matching_banda_larga.csv"
)
fwrite(
  matched[, .(coorte_match, cnpj_raiz, treat = treat_match, weights, subclass)],
  matching_file, sep = ";", bom = TRUE
)

if (!all(sort(unique(units_all[g > 0, g])) %in% matching_cohorts)) {
  stop("Coorte inesperada na especificacao com todos os controles.")
}
if (!all(sort(unique(units_matched[g > 0, g])) %in% matching_cohorts)) {
  stop("Coorte inesperada na especificacao pareada.")
}

spec_labels <- c(
  all = "Todos os controles da Anatel",
  matched = matching_designs[[preferred_design]]$label
)

estimate_cs <- function(units, specification) {
  d <- merge(panel[, .(cnpj_raiz, ano, log_acessos)], units, by = "cnpj_raiz")
  d[, id_num := as.integer(factor(cnpj_raiz))]
  setorder(d, id_num, ano)

  fit <- att_gt(
    yname = "log_acessos", tname = "ano", idname = "id_num", gname = "g",
    data = as.data.frame(d), xformla = ~1,
    control_group = "nevertreated", base_period = "universal",
    panel = TRUE, allow_unbalanced_panel = TRUE, est_method = "dr",
    bstrap = FALSE, cband = FALSE, alp = .05, print_details = FALSE
  )
  es <- aggte(
    fit, type = "dynamic", min_e = -4, max_e = 1, na.rm = TRUE,
    bstrap = FALSE, cband = FALSE, alp = .05
  )
  dynamic <- data.table(
    especificacao = specification,
    tempo_relativo = as.integer(es$egt),
    att = as.numeric(es$att.egt),
    erro_padrao = as.numeric(es$se.egt)
  )
  dynamic[tempo_relativo == -1L, `:=`(att = 0, erro_padrao = 0)]
  dynamic[, `:=`(
    ic95_inferior = att - qnorm(.975) * erro_padrao,
    ic95_superior = att + qnorm(.975) * erro_padrao
  )]

  overall <- as.numeric(es$overall.att)
  overall_se <- as.numeric(es$overall.se)
  summary <- data.table(
    especificacao = specification,
    outcome = if (internet_only) {
      "log(1 + acessos de internet em dezembro)"
    } else "log(1 + acessos em dezembro)",
    att_agregado = overall,
    erro_padrao = overall_se,
    ic95_inferior = overall - qnorm(.975) * overall_se,
    ic95_superior = overall + qnorm(.975) * overall_se,
    p_valor = 2 * pnorm(-abs(overall / overall_se)),
    p_tendencias_pre = as.numeric(fit$Wpval),
    tratadas = uniqueN(units[g > 0, cnpj_raiz]),
    controles = uniqueN(units[g == 0, cnpj_raiz])
  )
  list(summary = summary, dynamic = dynamic)
}

# Preserva os pares de cada coorte na etapa econometrica. Estimar as coortes
# juntas faria o pacote usar os controles de 2025 tambem nas comparacoes de
# 2024, desfazendo o matching 1:1.
estimate_matched_cs <- function(parts, specification) {
  cohort_gt <- list()
  cohort_summary <- list()

  for (g0_chr in names(parts)) {
    g0 <- as.integer(g0_chr)
    m <- parts[[g0_chr]]
    units_g <- m[, .(
      cnpj_raiz,
      g = ifelse(treat_match == 1L, as.numeric(g0), 0)
    )]
    d <- merge(panel[, .(cnpj_raiz, ano, log_acessos)], units_g, by = "cnpj_raiz")
    d[, id_num := as.integer(factor(cnpj_raiz))]
    setorder(d, id_num, ano)

    fit <- att_gt(
      yname = "log_acessos", tname = "ano", idname = "id_num", gname = "g",
      data = as.data.frame(d), xformla = ~1,
      control_group = "nevertreated", base_period = "universal",
      panel = TRUE, allow_unbalanced_panel = TRUE, est_method = "dr",
      bstrap = FALSE, cband = FALSE, alp = .05, print_details = FALSE
    )
    es <- aggte(
      fit, type = "dynamic", min_e = -5, max_e = 1, na.rm = TRUE,
      bstrap = FALSE, cband = FALSE, alp = .05
    )
    gt <- data.table(
      coorte = g0, ano = as.integer(fit$t),
      tempo_relativo = as.integer(fit$t - g0),
      att = as.numeric(fit$att), erro_padrao = as.numeric(fit$se),
      tratadas = uniqueN(units_g[g > 0, cnpj_raiz])
    )
    cohort_gt[[g0_chr]] <- gt
    cohort_summary[[g0_chr]] <- data.table(
      coorte = g0, tratadas = uniqueN(units_g[g > 0, cnpj_raiz]),
      controles = uniqueN(units_g[g == 0, cnpj_raiz]),
      periodos_pos = sum(gt$tempo_relativo >= 0),
      att_agregado = as.numeric(es$overall.att),
      erro_padrao = as.numeric(es$overall.se),
      p_valor = 2 * pnorm(-abs(as.numeric(es$overall.att / es$overall.se))),
      p_tendencias_pre = as.numeric(fit$Wpval)
    )
  }

  gt_all <- rbindlist(cohort_gt)
  cohort_results <- rbindlist(cohort_summary)
  dynamic <- gt_all[tempo_relativo >= -4L & tempo_relativo <= 1L, {
    w <- tratadas / sum(tratadas)
    .(
      att = sum(w * att),
      erro_padrao = if (all(is.na(erro_padrao))) 0 else
        sqrt(sum((w[!is.na(erro_padrao)] * erro_padrao[!is.na(erro_padrao)])^2))
    )
  }, by = tempo_relativo]
  dynamic[, especificacao := specification]
  setcolorder(dynamic, c("especificacao", "tempo_relativo", "att", "erro_padrao"))
  dynamic[tempo_relativo == -1L, `:=`(att = 0, erro_padrao = 0)]
  dynamic[, `:=`(
    ic95_inferior = att - qnorm(.975) * erro_padrao,
    ic95_superior = att + qnorm(.975) * erro_padrao
  )]

  # Agrega os ATTs pos-tratamento ponderando pelo numero de tratadas-periodo.
  # Os erros por coorte ja incorporam a covariancia entre periodos; as coortes
  # usam empresas e controles disjuntos.
  cohort_results[, exposure := tratadas * periodos_pos]
  cohort_results[, overall_weight := exposure / sum(exposure)]
  overall <- cohort_results[, sum(overall_weight * att_agregado)]
  overall_se <- cohort_results[, sqrt(sum((overall_weight * erro_padrao)^2))]
  summary <- data.table(
    especificacao = specification,
    outcome = if (internet_only) {
      "log(1 + acessos de internet em dezembro)"
    } else "log(1 + acessos em dezembro)",
    att_agregado = overall, erro_padrao = overall_se,
    ic95_inferior = overall - qnorm(.975) * overall_se,
    ic95_superior = overall + qnorm(.975) * overall_se,
    p_valor = 2 * pnorm(-abs(overall / overall_se)),
    p_tendencias_pre = min(cohort_results$p_tendencias_pre),
    tratadas = sum(cohort_results$tratadas),
    controles = sum(cohort_results$controles)
  )
  list(
    summary = summary, dynamic = dynamic,
    cohort_results = cohort_results, group_time = gt_all
  )
}

est_all <- estimate_cs(units_all, spec_labels[["all"]])
message("Estimada: ", spec_labels[["all"]])
est_matched_variants <- lapply(names(matching_results), function(nm) {
  label <- matching_designs[[nm]]$label
  ans <- estimate_matched_cs(matching_results[[nm]]$parts, label)
  message("Estimada: ", label)
  ans
})
names(est_matched_variants) <- names(matching_results)
est_matched <- est_matched_variants[[preferred_design]]

# Resultados separados por modalidade. Na especificacao 1:1, cada tratada
# conserva exatamente o controle de sua subclasse no matching principal.
modality_levels <- c("Direta", "Indireta")
modality_labels <- c(Direta = "Apoio direto", Indireta = "Apoio indireto")

subset_matched_parts <- function(parts, treated_ids) {
  lapply(parts, function(m) {
    selected_subclasses <- m[
      treat_match == 1L & cnpj_raiz %chin% treated_ids, unique(subclass)
    ]
    m[subclass %in% selected_subclasses]
  })
}

modality_results <- lapply(modality_levels, function(modality0) {
  treated0 <- treated_modality[modalidade == modality0, .(cnpj_raiz, g)]
  units0 <- rbindlist(list(
    treated0,
    units_all[g == 0L, .(cnpj_raiz, g)]
  ))
  parts0 <- subset_matched_parts(
    matched_parts, treated0$cnpj_raiz
  )
  list(
    all = estimate_cs(units0, spec_labels[["all"]]),
    matched = estimate_matched_cs(parts0, spec_labels[["matched"]]),
    parts = parts0
  )
})
names(modality_results) <- modality_levels

modality_att <- rbindlist(lapply(modality_levels, function(modality0) {
  z <- modality_results[[modality0]]
  ans <- rbindlist(list(z$all$summary, z$matched$summary))
  ans[, modalidade := modality_labels[[modality0]]]
  setcolorder(ans, c("modalidade", setdiff(names(ans), "modalidade")))
  ans
}))
modality_att[, diagnostico_pre := fifelse(
  p_tendencias_pre < .05,
  "Rejeita tendencias paralelas a 5%; ATT nao deve ser interpretado causalmente",
  "Nao rejeita tendencias paralelas a 5%"
)]
modality_dynamic <- rbindlist(lapply(modality_levels, function(modality0) {
  z <- modality_results[[modality0]]
  ans <- rbindlist(list(z$all$dynamic, z$matched$dynamic))
  ans[, modalidade := modality_labels[[modality0]]]
  ans
}))
modality_cohorts <- rbindlist(lapply(modality_levels, function(modality0) {
  ans <- copy(modality_results[[modality0]]$matched$cohort_results)
  ans[, modalidade := modality_labels[[modality0]]]
  setcolorder(ans, c("modalidade", setdiff(names(ans), "modalidade")))
  ans
}))
modality_pairs <- rbindlist(lapply(modality_levels, function(modality0) {
  ans <- rbindlist(modality_results[[modality0]]$parts)
  ans[, .(
    modalidade = modality_labels[[modality0]], coorte_match,
    cnpj_raiz, treat = treat_match, subclass
  )]
}))

cohort_results_all <- rbindlist(lapply(names(est_matched_variants), function(nm) {
  z <- copy(est_matched_variants[[nm]]$cohort_results)
  z[, `:=`(
    desenho = nm,
    especificacao = matching_designs[[nm]]$label
  )]
  setcolorder(z, c("desenho", "especificacao"))
  z
}))

att_summary <- rbindlist(c(
  list(est_all$summary),
  lapply(est_matched_variants, `[[`, "summary")
))
att_summary[, diagnostico_pre := fifelse(
  p_tendencias_pre < .05,
  "Rejeita tendencias paralelas a 5%; ATT nao deve ser interpretado causalmente",
  "Nao rejeita tendencias paralelas a 5%"
)]
dynamic <- rbindlist(c(
  list(est_all$dynamic),
  lapply(est_matched_variants, `[[`, "dynamic")
))
all_spec_labels <- c(
  spec_labels[["all"]],
  vapply(matching_designs, `[[`, character(1), "label")
)
dynamic[, especificacao := factor(especificacao, levels = all_spec_labels)]
dynamic_main <- dynamic[as.character(especificacao) %chin% unname(spec_labels)]

pd <- position_dodge(width = .18)
p <- ggplot(dynamic_main, aes(
  tempo_relativo, att, color = especificacao, group = especificacao
)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .35) +
  geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
  geom_errorbar(
    aes(ymin = ic95_inferior, ymax = ic95_superior),
    width = .07, position = pd, linewidth = .5
  ) +
  geom_point(position = pd, size = 2) +
  scale_x_continuous(breaks = -4:1) +
  scale_color_manual(values = c(cor_azul, cor_verde)) +
  labs(x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
  tema_bndes(legend_position = "bottom", base_size = 11)
ggsave(figure_file, p, width = 8.5, height = 5.2, dpi = 300, bg = cor_fundo)

modality_dynamic[, `:=`(
  modalidade = factor(
    modalidade, levels = unname(modality_labels[modality_levels])
  ),
  especificacao = factor(especificacao, levels = unname(spec_labels))
)]
p_modality <- ggplot(modality_dynamic, aes(
  tempo_relativo, att, color = especificacao, group = especificacao
)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = .35) +
  geom_vline(xintercept = -1, color = "grey65", linetype = "dashed", linewidth = .35) +
  geom_errorbar(
    aes(ymin = ic95_inferior, ymax = ic95_superior),
    width = .07, position = pd, linewidth = .5
  ) +
  geom_point(position = pd, size = 2) +
  facet_wrap(~modalidade, nrow = 1) +
  scale_x_continuous(breaks = -4:1) +
  scale_color_manual(values = c(cor_azul, cor_verde)) +
  labs(x = "Tempo relativo ao primeiro apoio FUST", y = "ATT") +
  tema_bndes(legend_position = "bottom", base_size = 11)
ggsave(
  modality_figure_file, p_modality,
  width = 10.5, height = 5.2, dpi = 300, bg = cor_fundo
)

annual_coverage <- panel[, .(
  prestadoras_ativas = uniqueN(cnpj_raiz[observada_no_ano == TRUE]),
  prestadoras_com_acessos_dezembro = uniqueN(cnpj_raiz[acessos > 0]),
  acessos_dezembro = sum(acessos),
  tratadas_ativas = uniqueN(cnpj_raiz[
    observada_no_ano == TRUE & cnpj_raiz %chin% official_treated$cnpj_raiz
  ]),
  controles_rais_ativos = uniqueN(cnpj_raiz[
    observada_no_ano == TRUE & cnpj_raiz %chin% official[treat == 0L, cnpj_raiz]
  ])
), by = ano]

sample_audit <- data.table(
  etapa = c(
    "Amostra oficial RAIS",
    "Observadas na Anatel - todos os controles",
    "Especificacao: todos os controles",
    paste0("Especificacao: ", vapply(matching_designs, `[[`, character(1), "label"))
  ),
  tratadas = c(
    uniqueN(official[treat == 1L, cnpj_raiz]),
    nrow(treated_anatel),
    uniqueN(units_all[g > 0, cnpj_raiz]),
    vapply(matching_results, function(z) {
      uniqueN(rbindlist(z$parts)[treat_match == 1L, cnpj_raiz])
    }, integer(1))
  ),
  controles = c(
    uniqueN(official[treat == 0L, cnpj_raiz]),
    length(all_control_roots),
    uniqueN(units_all[g == 0L, cnpj_raiz]),
    vapply(matching_results, function(z) {
      uniqueN(rbindlist(z$parts)[treat_match == 0L, cnpj_raiz])
    }, integer(1))
  )
)

definitions <- data.table(
  item = c(
    "Unidade", "Filtro de produto", "Elegibilidade do CNPJ", "Tratamento",
    "Outcome", "Periodo", "Controles - universo", "Controles - pareada",
    "Estimador", "Inferencia", "Ano de 2026"
  ),
  definicao = c(
    "Raiz do CNPJ (8 digitos)",
    if (internet_only) {
      "Somente linhas com Tipo de Produto igual a INTERNET"
    } else "Todos os tipos de produto da base de banda larga fixa",
    if (internet_only) {
      "Mantem somente raizes de CNPJ com acessos de INTERNET positivos em pelo menos um mes de 2021-2025"
    } else "Mantem prestadoras com acessos positivos em pelo menos um mes de 2020-2025",
    "Primeiro apoio FUST; coortes 2024 e 2025 da amostra oficial S4 da RAIS",
    if (internet_only) {
      "log(1 + estoque de acessos classificados como INTERNET em dezembro)"
    } else "log(1 + estoque de acessos de banda larga fixa em dezembro)",
    paste0(min(years), " a ", max(years)),
    paste0(
      "Todas as prestadoras elegiveis ativas em algum mes de ", min(years),
      "-", max(years), " que nunca receberam FUST"
    ),
    paste0(
      "1:1 sem reposicao e sem exact; desenhos alternativos avaliados sem usar acessos na selecao; principal escolhido por balanceamento RAIS: ",
      matching_designs[[preferred_design]]$label
    ),
    "Callaway-Sant'Anna, grupo de controle nunca tratado, DR, periodo-base universal",
    "Intervalos pontuais de 95%; sem bootstrap, como na figura principal da RAIS",
    "Excluido por estar incompleto (dados somente ate julho)"
  )
)

wb <- createWorkbook()
addWorksheet(wb, "ATT agregado")
writeDataTable(wb, "ATT agregado", att_summary)
addWorksheet(wb, "Event study")
writeDataTable(wb, "Event study", dynamic)
addWorksheet(wb, "ATT por modalidade")
writeDataTable(wb, "ATT por modalidade", modality_att)
addWorksheet(wb, "Event study modalidade")
writeDataTable(wb, "Event study modalidade", modality_dynamic)
addWorksheet(wb, "Coortes modalidade")
writeDataTable(wb, "Coortes modalidade", modality_cohorts)
addWorksheet(wb, "Pares por modalidade")
writeDataTable(wb, "Pares por modalidade", modality_pairs)
addWorksheet(wb, "Amostras")
writeDataTable(wb, "Amostras", sample_audit)
addWorksheet(wb, "Balanceamento 1x1")
writeDataTable(wb, "Balanceamento 1x1", matching_balance)
addWorksheet(wb, "Selecao matching")
writeDataTable(wb, "Selecao matching", merge(
  matching_ranking, matching_choices, by = c("desenho", "descricao"), all.x = TRUE
))
addWorksheet(wb, "Resultados por coorte")
writeDataTable(wb, "Resultados por coorte", est_matched$cohort_results)
addWorksheet(wb, "Coortes - alternativas")
writeDataTable(wb, "Coortes - alternativas", cohort_results_all)
addWorksheet(wb, "ATT grupo-tempo 1x1")
writeDataTable(wb, "ATT grupo-tempo 1x1", est_matched$group_time)
addWorksheet(wb, "Pares 1x1")
writeDataTable(
  wb, "Pares 1x1",
  matched[, .(coorte_match, cnpj_raiz, treat = treat_match, subclass)]
)
all_pairs <- rbindlist(lapply(names(matching_results), function(nm) {
  z <- rbindlist(matching_results[[nm]]$parts)
  z[, .(
    desenho = nm, descricao = matching_designs[[nm]]$label,
    coorte_match, cnpj_raiz, treat = treat_match, subclass
  )]
}))
addWorksheet(wb, "Pares - alternativas")
writeDataTable(wb, "Pares - alternativas", all_pairs)
addWorksheet(wb, "Cobertura anual")
writeDataTable(wb, "Cobertura anual", annual_coverage)
addWorksheet(wb, "Definicoes")
writeDataTable(wb, "Definicoes", definitions)
for (sheet in names(wb)) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:20, widths = "auto")
}
saveWorkbook(wb, results_file, overwrite = TRUE)

print(att_summary)
print(sample_audit)
cat("\nFigura: ", figure_file, "\n", sep = "")
cat("Resultados: ", results_file, "\n", sep = "")
