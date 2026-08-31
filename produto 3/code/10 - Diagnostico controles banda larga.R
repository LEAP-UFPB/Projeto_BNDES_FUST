#!/usr/bin/env Rscript

# Diagnostico exploratorio da composicao de controles que reproduz bem a
# trajetoria pre-Anatel. Essa amostra NAO e uma especificacao causal final: ela
# usa o outcome pre-tratamento apenas como benchmark para investigar quais
# caracteristicas observadas na RAIS distinguem seus controles.

suppressPackageStartupMessages({
  library(data.table)
  library(MatchIt)
  library(openxlsx)
})

args <- commandArgs(FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) stop("Execute este arquivo com Rscript.")
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
source(file.path(dirname(script), "08 - Acessos banda larga fixa.R"), encoding = "UTF-8")

diagnostic_file <- file.path(
  output_dir, "Diagnostico - Controles resultado bom banda larga.xlsx"
)
rais_full <- fread(
  file.path(data_dir, "produto3_caracteristicas_rais_banda_larga.csv"),
  sep = ";", colClasses = c(cnpj_raiz = "character")
)
rais_full[, cnpj_raiz := clean_root(cnpj_raiz)]
anatel_structure <- fread(
  file.path(data_dir, "produto3_covariaveis_estruturais_anatel.csv"),
  sep = ";", colClasses = c(cnpj_raiz = "character")
)
anatel_structure[, cnpj_raiz := clean_root(cnpj_raiz)]
full_profile_vars <- c(
  "prop_superior", "tempo_medio_trabalhadores",
  "idade_media_trabalhadores", "prop_homens", "prop_pretos"
)
full_category_vars <- c(
  "uf_principal", "municipio_principal", "division",
  "tamanho_estab_principal"
)

run_outcome_benchmark <- function() {
  used_controls <- character()
  parts <- list()
  choices <- list()
  balance <- list()

  for (g0 in matching_cohorts) {
    treated_ids <- treated_anatel[g == g0, cnpj_raiz]
    roots <- c(treated_ids, setdiff(all_control_roots, used_controls))
    pre_years <- (g0 - 4L):(g0 - 1L)
    wide <- dcast(
      panel[ano %in% pre_years, .(
        cnpj_raiz, variavel = paste0("log_acessos_", ano), log_acessos
      )],
      cnpj_raiz ~ variavel, value.var = "log_acessos", fill = 0
    )
    pool <- merge(data.table(cnpj_raiz = unique(roots)), wide,
                  by = "cnpj_raiz", all.x = TRUE)
    pre_vars <- paste0("log_acessos_", pre_years)
    for (v in pre_vars) set(pool, which(is.na(pool[[v]])), v, 0)
    pool[, treat_match := as.integer(cnpj_raiz %chin% treated_ids)]

    candidate_specs <- rbindlist(list(
      data.table(order = c("farthest", "closest"), seed = 0L),
      data.table(order = "random", seed = 1:100)
    ))
    candidates <- lapply(seq_len(nrow(candidate_specs)), function(i) {
      z <- candidate_specs[i]
      set.seed(3854035 + z$seed)
      fit <- tryCatch(matchit(
        reformulate(pre_vars, "treat_match"), data = pool,
        method = "nearest", distance = "mahalanobis", ratio = 1,
        replace = FALSE, m.order = z$order
      ), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      m <- as.data.table(match.data(fit, drop.unmatched = TRUE))
      smds <- vapply(pre_vars, function(v) smd_value(m, v), numeric(1))
      list(m = m, score = max(abs(smds)), order = z$order, seed = z$seed)
    })
    candidates <- Filter(Negate(is.null), candidates)
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "score"))]]
    m <- best$m
    m[, coorte_match := g0]
    parts[[as.character(g0)]] <- m
    used_controls <- union(used_controls, m[treat_match == 0L, cnpj_raiz])
    choices[[as.character(g0)]] <- data.table(
      coorte = g0, pares = length(treated_ids), ordem = best$order,
      seed = best$seed, maior_abs_smd_pre_acessos = best$score
    )
    balance[[as.character(g0)]] <- rbindlist(lapply(pre_vars, function(v) {
      data.table(
        coorte = g0, variavel = v,
        media_tratadas = mean(m[treat_match == 1L, get(v)]),
        media_controles = mean(m[treat_match == 0L, get(v)]),
        smd = smd_value(m, v)
      )
    }))
  }
  list(
    parts = parts, choices = rbindlist(choices), balance = rbindlist(balance)
  )
}

benchmark <- run_outcome_benchmark()
benchmark_est <- estimate_matched_cs(
  benchmark$parts, "Benchmark diagnostico: trajetoria pre-Anatel"
)

make_rais_characteristics <- function(g0, ids) {
  base <- data.table(cnpj_raiz = unique(ids))
  h <- employment_history[
    cnpj_raiz %chin% ids & ano %in% (g0 - 3L):(g0 - 1L),
    .(cnpj_raiz, variavel = paste0("log_emprego_t", g0 - ano),
      valor = log1p(emprego))
  ]
  h <- dcast(h, cnpj_raiz ~ variavel, value.var = "valor", fill = 0)
  salary <- rais_matching[ano == g0 - 1L & cnpj_raiz %chin% ids, .(
    cnpj_raiz, log_salario_medio_t1 = log1p(fcoalesce(salario_medio, 0))
  )]
  rich_cols <- c("cnpj_raiz", full_profile_vars, full_category_vars)
  rich <- unique(
    rais_full[ano == g0 - 1L & cnpj_raiz %chin% ids, ..rich_cols],
    by = "cnpj_raiz"
  )
  ans <- merge(base, h, by = "cnpj_raiz", all.x = TRUE)
  ans <- merge(ans, salary, by = "cnpj_raiz", all.x = TRUE)
  ans <- merge(ans, rich, by = "cnpj_raiz", all.x = TRUE)
  for (v in common_balance_vars) set(ans, which(is.na(ans[[v]])), v, 0)
  ans
}

numeric_comparisons <- list()
categorical_comparisons <- list()
pair_comparisons <- list()
characteristics_rows <- list()

for (g0_chr in names(benchmark$parts)) {
  g0 <- as.integer(g0_chr)
  m <- copy(benchmark$parts[[g0_chr]])
  chars <- make_rais_characteristics(g0, m$cnpj_raiz)
  m <- merge(m[, .(cnpj_raiz, treat_match, subclass)], chars,
             by = "cnpj_raiz", all.x = TRUE)
  characteristics_rows[[g0_chr]] <- copy(m)[, coorte := g0]

  numeric_vars <- c(common_balance_vars, full_profile_vars)
  numeric_comparisons[[g0_chr]] <- rbindlist(lapply(numeric_vars, function(v) {
    tr <- m[treat_match == 1L & is.finite(get(v)), get(v)]
    ct <- m[treat_match == 0L & is.finite(get(v)), get(v)]
    den <- sqrt((var(tr) + var(ct)) / 2)
    data.table(
      coorte = g0, variavel = v,
      n_tratadas = length(tr), n_controles = length(ct),
      media_tratadas = mean(tr), media_controles = mean(ct),
      diferenca = mean(tr) - mean(ct),
      smd = ifelse(is.finite(den) && den > 0,
                   (mean(tr) - mean(ct)) / den, 0)
    )
  }))

  categorical_vars <- c(
    "uf_principal", "municipio_principal", "division",
    "tamanho_estab_principal"
  )
  categorical_comparisons[[g0_chr]] <- rbindlist(lapply(
    categorical_vars, function(v) {
      lev <- union(
        unique(m[treat_match == 1L, as.character(get(v))]),
        unique(m[treat_match == 0L, as.character(get(v))])
      )
      rbindlist(lapply(lev, function(x) data.table(
        coorte = g0, variavel = v, categoria = x,
        prop_tratadas = mean(m[treat_match == 1L, as.character(get(v)) == x], na.rm = TRUE),
        prop_controles = mean(m[treat_match == 0L, as.character(get(v)) == x], na.rm = TRUE)
      )))
    }
  ))

  treated_pairs <- m[treat_match == 1L]
  control_pairs <- m[treat_match == 0L]
  pair_comparisons[[g0_chr]] <- merge(
    treated_pairs[, .(
      subclass, cnpj_tratada = cnpj_raiz,
      uf_t = uf_principal, municipio_t = municipio_principal,
      divisao_t = division, tamanho_t = tamanho_estab_principal,
      setor_t = division
    )],
    control_pairs[, .(
      subclass, cnpj_controle = cnpj_raiz,
      uf_c = uf_principal, municipio_c = municipio_principal,
      divisao_c = division, tamanho_c = tamanho_estab_principal,
      setor_c = division
    )], by = "subclass", all = TRUE
  )[, `:=`(
    coorte = g0,
    mesma_uf = uf_t == uf_c,
    mesmo_municipio = municipio_t == municipio_c,
    mesma_divisao = divisao_t == divisao_c,
    mesmo_tamanho = tamanho_t == tamanho_c,
    mesmo_setor = setor_t == setor_c
  )]
}

numeric_comparisons <- rbindlist(numeric_comparisons)
categorical_comparisons <- rbindlist(categorical_comparisons)
pair_comparisons <- rbindlist(pair_comparisons, fill = TRUE)
characteristics_rows <- rbindlist(characteristics_rows, fill = TRUE)
pair_summary <- pair_comparisons[, .(
  pares = .N,
  prop_mesma_uf = mean(mesma_uf, na.rm = TRUE),
  prop_mesmo_municipio = mean(mesmo_municipio, na.rm = TRUE),
  prop_mesma_divisao = mean(mesma_divisao, na.rm = TRUE),
  prop_mesmo_tamanho = mean(mesmo_tamanho, na.rm = TRUE),
  prop_mesmo_setor = mean(mesmo_setor, na.rm = TRUE)
), by = coorte]

# Teste consequente ao diagnostico: matching que usa somente a tendencia da
# RAIS, sem nivel de acessos e sem qualquer variavel da Anatel. Todas as
# alternativas abaixo sao preservadas; nenhuma e escolhida pelo resultado pos.
rais_trend_designs <- list(
  crescimento_mahalanobis = list(
    label = "RAIS-only: crescimento do emprego (Mahalanobis)",
    vars = c("delta_emprego_32", "delta_emprego_21"),
    distance = "mahalanobis", orders = c("farthest", "closest")
  ),
  crescimento_propensity = list(
    label = "RAIS-only: crescimento do emprego (propensity score)",
    vars = c("delta_emprego_32", "delta_emprego_21"),
    distance = "glm", orders = c("largest", "smallest", "closest", "farthest")
  ),
  crescimento_salario_mahalanobis = list(
    label = "RAIS-only: crescimento do emprego e salario (Mahalanobis)",
    vars = c("delta_emprego_32", "delta_emprego_21", "log_salario_medio_t1"),
    distance = "mahalanobis", orders = c("farthest", "closest")
  ),
  crescimento_perfil_mahalanobis = list(
    label = "RAIS-only: crescimento e perfil dos trabalhadores (Mahalanobis)",
    vars = c("delta_emprego_32", "delta_emprego_21", full_profile_vars),
    distance = "mahalanobis", orders = c("farthest", "closest")
  ),
  crescimento_perfil_propensity = list(
    label = "RAIS-only: crescimento e perfil dos trabalhadores (propensity score)",
    vars = c("delta_emprego_32", "delta_emprego_21", full_profile_vars),
    distance = "glm", orders = c("largest", "smallest", "closest", "farthest")
  ),
  nivel_trajetoria_perfil_mahalanobis = list(
    label = "RAIS-only: nivel, trajetoria e perfil completo (Mahalanobis)",
    vars = c(common_balance_vars, full_profile_vars),
    distance = "mahalanobis", orders = c("farthest", "closest")
  )
)

for (nm in names(rais_trend_designs)) {
  if (is.null(rais_trend_designs[[nm]]$formula_terms)) {
    rais_trend_designs[[nm]]$formula_terms <- rais_trend_designs[[nm]]$vars
  }
  if (is.null(rais_trend_designs[[nm]]$categorical_vars)) {
    rais_trend_designs[[nm]]$categorical_vars <- character()
  }
  if (is.null(rais_trend_designs[[nm]]$exact_terms)) {
    rais_trend_designs[[nm]]$exact_terms <- character()
  }
}

run_rais_trend_design <- function(design_name, design) {
  used_controls <- character()
  parts <- list()
  balance <- list()
  choices <- list()
  for (g0 in matching_cohorts) {
    treated_ids <- treated_anatel[g == g0, cnpj_raiz]
    pool <- make_matching_pool(g0, treated_ids, used_controls)
    profile <- rais_full[ano == g0 - 1L, c(
      "cnpj_raiz", full_profile_vars, full_category_vars
    ), with = FALSE]
    profile <- unique(profile, by = "cnpj_raiz")
    pool <- merge(pool, profile, by = "cnpj_raiz", all.x = TRUE)
    for (v in full_profile_vars) {
      replacement <- median(pool[[v]], na.rm = TRUE)
      if (!is.finite(replacement)) replacement <- 0
      set(pool, which(!is.finite(pool[[v]])), v, replacement)
    }
    for (v in full_category_vars) {
      set(pool, which(is.na(pool[[v]]) | !nzchar(as.character(pool[[v]]))), v, "Ignorado")
    }
    pool[, `:=`(
      delta_emprego_32 = log_emprego_t2 - log_emprego_t3,
      delta_emprego_21 = log_emprego_t1 - log_emprego_t2
    )]
    candidate_specs <- rbindlist(list(
      data.table(order = design$orders, seed = 0L),
      data.table(order = "random", seed = 1:100)
    ))
    candidates <- lapply(seq_len(nrow(candidate_specs)), function(i) {
      z <- candidate_specs[i]
      set.seed(8102026 + z$seed)
      args_match <- list(
        formula = reformulate(design$formula_terms, "treat_match"), data = pool,
        method = "nearest", distance = design$distance, ratio = 1,
        replace = FALSE, m.order = z$order
      )
      if (design$distance == "glm") args_match$link <- "logit"
      if (length(design$exact_terms)) {
        args_match$exact <- reformulate(design$exact_terms)
      }
      fit <- tryCatch(do.call(matchit, args_match), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      m <- as.data.table(match.data(fit, drop.unmatched = TRUE))
      smds <- vapply(design$vars, function(v) smd_value(m, v), numeric(1))
      category_differences <- unlist(lapply(design$categorical_vars, function(v) {
        lev <- union(
          unique(m[treat_match == 1L, as.character(get(v))]),
          unique(m[treat_match == 0L, as.character(get(v))])
        )
        vapply(lev, function(x) {
          mean(m[treat_match == 1L, as.character(get(v)) == x]) -
            mean(m[treat_match == 0L, as.character(get(v)) == x])
        }, numeric(1))
      }))
      list(
        m = m, score = max(abs(c(smds, category_differences))),
        order = z$order, seed = z$seed
      )
    })
    candidates <- Filter(Negate(is.null), candidates)
    if (!length(candidates)) stop("Sem matching valido em ", design_name, "/", g0)
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "score"))]]
    m <- best$m
    m[, coorte_match := g0]
    parts[[as.character(g0)]] <- m
    used_controls <- union(used_controls, m[treat_match == 0L, cnpj_raiz])
    choices[[as.character(g0)]] <- data.table(
      desenho = design_name, especificacao = design$label, coorte = g0,
      ordem = best$order, seed = best$seed, maior_abs_smd = best$score
    )
    balance[[as.character(g0)]] <- rbindlist(lapply(design$vars, function(v) {
      data.table(
        desenho = design_name, especificacao = design$label, coorte = g0,
        variavel = v,
        media_tratadas = mean(m[treat_match == 1L, get(v)]),
        media_controles = mean(m[treat_match == 0L, get(v)]),
        smd = smd_value(m, v)
      )
    }))
  }
  list(parts = parts, choices = rbindlist(choices), balance = rbindlist(balance))
}

rais_trend_matches <- Map(
  run_rais_trend_design, names(rais_trend_designs), rais_trend_designs
)
rais_trend_estimates <- lapply(names(rais_trend_matches), function(nm) {
  estimate_matched_cs(
    rais_trend_matches[[nm]]$parts, rais_trend_designs[[nm]]$label
  )
})
names(rais_trend_estimates) <- names(rais_trend_matches)
rais_trend_summary <- rbindlist(lapply(names(rais_trend_estimates), function(nm) {
  z <- copy(rais_trend_estimates[[nm]]$summary)
  z[, desenho := nm]
  setcolorder(z, "desenho")
  z
}))
rais_trend_cohorts <- rbindlist(lapply(names(rais_trend_estimates), function(nm) {
  z <- copy(rais_trend_estimates[[nm]]$cohort_results)
  z[, `:=`(desenho = nm, especificacao = rais_trend_designs[[nm]]$label)]
  setcolorder(z, c("desenho", "especificacao"))
  z
}))
rais_trend_balance <- rbindlist(lapply(rais_trend_matches, `[[`, "balance"))
rais_trend_choices <- rbindlist(lapply(rais_trend_matches, `[[`, "choices"))

# Matching combinado: caracteristicas operacionais da base de banda larga sem
# usar nenhuma coluna mensal de acessos, com ou sem covariaveis da RAIS.
structure_level_vars <- c(
  "log_municipios_t1", "log_ufs_t1", "log_tecnologias_t1",
  "log_meios_t1", "log_faixas_t1", "log_ofertas_t1",
  "log_velocidade_mediana_t1", "log_velocidade_maxima_t1",
  "porte_pequeno_t1", "pj_t1", "fibra_t1", "radio_t1",
  "cabo_t1", "satelite_t1"
)
structure_history_vars <- c(
  paste0("log_municipios_t", 3:1),
  paste0("log_ofertas_t", 3:1),
  paste0("log_velocidade_maxima_t", 3:1),
  "log_tecnologias_t1", "log_meios_t1", "log_faixas_t1",
  "porte_pequeno_t1", "pj_t1", "fibra_t1", "radio_t1",
  "cabo_t1", "satelite_t1"
)

make_structure_pool <- function(g0, treated_ids, used_controls) {
  roots <- unique(c(treated_ids, setdiff(all_control_roots, used_controls)))
  base <- data.table(cnpj_raiz = roots)
  z <- copy(anatel_structure[ano %in% (g0 - 3L):(g0 - 1L)])
  z[, lag := g0 - ano]
  z[, `:=`(
    log_municipios = log1p(n_municipios),
    log_ufs = log1p(n_ufs),
    log_tecnologias = log1p(n_tecnologias),
    log_meios = log1p(n_meios_acesso),
    log_faixas = log1p(n_faixas_velocidade),
    log_ofertas = log1p(n_ofertas_catalogo),
    log_velocidade_mediana = log1p(velocidade_mediana),
    log_velocidade_maxima = log1p(velocidade_maxima),
    pj = oferece_pessoa_juridica,
    fibra = oferece_fibra,
    radio = oferece_radio,
    cabo = oferece_cabo,
    satelite = oferece_satelite
  )]
  value_vars <- c(
    "log_municipios", "log_ufs", "log_tecnologias", "log_meios",
    "log_faixas", "log_ofertas", "log_velocidade_mediana",
    "log_velocidade_maxima", "porte_pequeno", "pj", "fibra", "radio",
    "cabo", "satelite"
  )
  long <- melt(
    z, id.vars = c("cnpj_raiz", "lag"), measure.vars = value_vars,
    variable.name = "feature", value.name = "value"
  )
  long[, feature_lag := paste0(feature, "_t", lag)]
  wide <- dcast(long, cnpj_raiz ~ feature_lag, value.var = "value", fill = 0)
  pool <- merge(base, wide, by = "cnpj_raiz", all.x = TRUE)
  required <- unique(c(structure_level_vars, structure_history_vars))
  for (v in required) {
    if (!v %in% names(pool)) pool[, (v) := 0]
    set(pool, which(!is.finite(pool[[v]])), v, 0)
  }

  rais <- make_matching_pool(g0, treated_ids, used_controls)
  rais[, `:=`(
    delta_emprego_32 = log_emprego_t2 - log_emprego_t3,
    delta_emprego_21 = log_emprego_t1 - log_emprego_t2
  )]
  profile <- unique(rais_full[ano == g0 - 1L, c(
    "cnpj_raiz", full_profile_vars
  ), with = FALSE], by = "cnpj_raiz")
  rais <- merge(rais, profile, by = "cnpj_raiz", all.x = TRUE)
  for (v in full_profile_vars) {
    replacement <- median(rais[[v]], na.rm = TRUE)
    if (!is.finite(replacement)) replacement <- 0
    set(rais, which(!is.finite(rais[[v]])), v, replacement)
  }
  pool <- merge(
    pool,
    rais[, c(
      "cnpj_raiz", "log_emprego_t3", "log_emprego_t2", "log_emprego_t1",
      "log_salario_medio_t1", "delta_emprego_32", "delta_emprego_21",
      full_profile_vars
    ), with = FALSE],
    by = "cnpj_raiz", all.x = TRUE
  )
  pool[, treat_match := as.integer(cnpj_raiz %chin% treated_ids)]
  pool
}

combined_designs <- list(
  estrutura_nivel = list(
    label = "Banda larga sem acessos: estrutura em g-1",
    vars = structure_level_vars
  ),
  estrutura_trajetoria = list(
    label = "Banda larga sem acessos: trajetoria estrutural pre",
    vars = structure_history_vars
  ),
  estrutura_nivel_emprego = list(
    label = "Banda larga sem acessos em g-1 + crescimento do emprego RAIS",
    vars = c(structure_level_vars, "delta_emprego_32", "delta_emprego_21")
  ),
  estrutura_nivel_rais = list(
    label = "Banda larga sem acessos em g-1 + perfil RAIS",
    vars = c(
      structure_level_vars, "delta_emprego_32", "delta_emprego_21",
      "log_salario_medio_t1", full_profile_vars
    )
  ),
  estrutura_rais = list(
    label = "Banda larga sem acessos + RAIS",
    vars = c(
      structure_history_vars, "delta_emprego_32", "delta_emprego_21",
      "log_salario_medio_t1", full_profile_vars
    )
  )
)

run_combined_design <- function(design_name, design) {
  used_controls <- character()
  parts <- list()
  balance <- list()
  choices <- list()
  for (g0 in matching_cohorts) {
    treated_ids <- treated_anatel[g == g0, cnpj_raiz]
    pool <- make_structure_pool(g0, treated_ids, used_controls)
    active_vars <- design$vars[vapply(design$vars, function(v) {
      s <- sd(pool[[v]], na.rm = TRUE)
      is.finite(s) && s > 1e-10
    }, logical(1))]
    candidate_specs <- rbindlist(list(
      data.table(order = c("farthest", "closest"), seed = 0L),
      data.table(order = "random", seed = 1:100)
    ))
    candidates <- lapply(seq_len(nrow(candidate_specs)), function(i) {
      q <- candidate_specs[i]
      set.seed(13102026 + q$seed)
      fit <- tryCatch(matchit(
        reformulate(active_vars, "treat_match"), data = pool,
        method = "nearest", distance = "mahalanobis", ratio = 1,
        replace = FALSE, m.order = q$order
      ), error = function(e) NULL)
      if (is.null(fit)) return(NULL)
      m <- as.data.table(match.data(fit, drop.unmatched = TRUE))
      smds <- vapply(active_vars, function(v) smd_value(m, v), numeric(1))
      list(m = m, score = max(abs(smds)), order = q$order, seed = q$seed)
    })
    candidates <- Filter(Negate(is.null), candidates)
    if (!length(candidates)) stop("Sem matching combinado em ", design_name, "/", g0)
    best <- candidates[[which.min(vapply(candidates, `[[`, numeric(1), "score"))]]
    m <- best$m
    m[, coorte_match := g0]
    parts[[as.character(g0)]] <- m
    used_controls <- union(used_controls, m[treat_match == 0L, cnpj_raiz])
    choices[[as.character(g0)]] <- data.table(
      desenho = design_name, especificacao = design$label, coorte = g0,
      ordem = best$order, seed = best$seed, maior_abs_smd = best$score,
      covariaveis_ativas = length(active_vars)
    )
    balance[[as.character(g0)]] <- rbindlist(lapply(active_vars, function(v) {
      data.table(
        desenho = design_name, especificacao = design$label, coorte = g0,
        variavel = v,
        media_tratadas = mean(m[treat_match == 1L, get(v)]),
        media_controles = mean(m[treat_match == 0L, get(v)]),
        smd = smd_value(m, v)
      )
    }))
  }
  list(parts = parts, balance = rbindlist(balance), choices = rbindlist(choices))
}

combined_matches <- Map(
  run_combined_design, names(combined_designs), combined_designs
)
combined_estimates <- lapply(names(combined_matches), function(nm) {
  estimate_matched_cs(combined_matches[[nm]]$parts, combined_designs[[nm]]$label)
})
names(combined_estimates) <- names(combined_matches)
combined_summary <- rbindlist(lapply(names(combined_estimates), function(nm) {
  z <- copy(combined_estimates[[nm]]$summary)
  z[, desenho := nm]
  setcolorder(z, "desenho")
  z
}))
combined_cohorts <- rbindlist(lapply(names(combined_estimates), function(nm) {
  z <- copy(combined_estimates[[nm]]$cohort_results)
  z[, `:=`(desenho = nm, especificacao = combined_designs[[nm]]$label)]
  setcolorder(z, c("desenho", "especificacao"))
  z
}))
combined_balance <- rbindlist(lapply(combined_matches, `[[`, "balance"))
combined_choices <- rbindlist(lapply(combined_matches, `[[`, "choices"))

wb <- createWorkbook()
for (sheet in c(
  "Resultado benchmark", "Event study benchmark", "Escolha benchmark",
  "Balanceamento acessos", "Comparacao numerica RAIS",
  "Comparacao categorias RAIS", "Resumo pares", "Pares benchmark",
  "Caracteristicas RAIS", "Resultados RAIS-only", "Coortes RAIS-only",
  "Balance RAIS-only", "Escolha RAIS-only", "Resultados BL+RAIS",
  "Coortes BL+RAIS", "Balance BL+RAIS", "Escolha BL+RAIS"
)) addWorksheet(wb, sheet)
writeDataTable(wb, "Resultado benchmark", benchmark_est$summary)
writeDataTable(wb, "Event study benchmark", benchmark_est$dynamic)
writeDataTable(wb, "Escolha benchmark", benchmark$choices)
writeDataTable(wb, "Balanceamento acessos", benchmark$balance)
writeDataTable(wb, "Comparacao numerica RAIS", numeric_comparisons)
writeDataTable(wb, "Comparacao categorias RAIS", categorical_comparisons)
writeDataTable(wb, "Resumo pares", pair_summary)
writeDataTable(wb, "Pares benchmark", pair_comparisons)
writeDataTable(wb, "Caracteristicas RAIS", characteristics_rows)
writeDataTable(wb, "Resultados RAIS-only", rais_trend_summary)
writeDataTable(wb, "Coortes RAIS-only", rais_trend_cohorts)
writeDataTable(wb, "Balance RAIS-only", rais_trend_balance)
writeDataTable(wb, "Escolha RAIS-only", rais_trend_choices)
writeDataTable(wb, "Resultados BL+RAIS", combined_summary)
writeDataTable(wb, "Coortes BL+RAIS", combined_cohorts)
writeDataTable(wb, "Balance BL+RAIS", combined_balance)
writeDataTable(wb, "Escolha BL+RAIS", combined_choices)
for (sheet in names(wb)) {
  freezePane(wb, sheet, firstRow = TRUE)
  setColWidths(wb, sheet, cols = 1:25, widths = "auto")
}
saveWorkbook(wb, diagnostic_file, overwrite = TRUE)

print(benchmark_est$summary)
print(benchmark_est$cohort_results)
print(numeric_comparisons[order(coorte, -abs(smd))])
print(pair_summary)
print(rais_trend_summary)
print(rais_trend_cohorts)
print(combined_summary)
print(combined_cohorts)
cat("\nDiagnostico: ", diagnostic_file, "\n", sep = "")
