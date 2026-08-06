#!/usr/bin/env Rscript

packages <- c("data.table", "readxl", "stringr", "did", "synthdid", "ggplot2", "writexl")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) stop("Pacotes R ausentes: ", paste(missing, collapse = ", "), call. = FALSE)

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(stringr)
  library(did)
  library(synthdid)
  library(ggplot2)
  library(writexl)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[1], fixed = TRUE) else default
}

all_args <- commandArgs(trailingOnly = FALSE)
script_arg <- all_args[grepl("^--file=", all_args)]
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/", mustWork = TRUE)
produto_dir <- normalizePath(file.path(dirname(script_path), "..", ".."), winslash = "/", mustWork = TRUE)
repo_root <- normalizePath(file.path(produto_dir, ".."), winslash = "/", mustWork = TRUE)

matched_file <- arg_value("matched", file.path(produto_dir, "analysis", "psm_did", "unidades_pareadas.csv"))
monthly_file <- arg_value("monthly", file.path(repo_root, "data", "RAIS_matching_emprego_mensal_2022_2024.xlsx"))
mass_file <- arg_value("mass", file.path(produto_dir, "analysis", "frequencias_pos_covid", "massa_salarial_mensal_2022_2024.csv"))
operations_file <- arg_value("operations", file.path(repo_root, "data", "Base de Operações_FUST_compilada.xlsx"))
fust_panel_file <- arg_value("fust-panel", file.path(repo_root, "data", "Painel_CNPJ_FUST_RAIS_2018_2024.xlsx"))
out_dir <- arg_value("out", file.path(produto_dir, "analysis", "frequencias_pos_covid"))
start_ym <- arg_value("inicio", "2022-01")
end_ym <- arg_value("fim", "2024-12")
bootstrap_reps <- as.integer(arg_value("bootstrap-reps", "0"))
sdid_reps <- as.integer(arg_value("sdid-reps", "50"))
seed <- as.integer(arg_value("seed", "3854035"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

clean_digits <- function(x) {
  x <- str_replace_all(as.character(x), "\\D", "")
  x[x == ""] <- NA_character_
  x
}
clean_root <- function(x) {
  x <- clean_digits(x)
  x <- str_pad(x, 8, side = "left", pad = "0")
  fifelse(nchar(x) > 8, substr(x, 1, 8), x)
}
normalize_name <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- toupper(x)
  str_squish(str_replace_all(x, "[^A-Z0-9]+", " "))
}
parse_num <- function(x) {
  x <- trimws(as.character(x))
  comma <- grepl(",", x, fixed = TRUE)
  x[comma] <- gsub(".", "", x[comma], fixed = TRUE)
  x[comma] <- gsub(",", ".", x[comma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}
parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  z <- as.character(x)
  out <- as.Date(rep(NA_character_, length(z)))
  dmy_slash <- grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", z)
  dmy_dash <- grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", z)
  out[dmy_slash] <- suppressWarnings(as.Date(z[dmy_slash], format = "%d/%m/%Y"))
  out[dmy_dash] <- suppressWarnings(as.Date(z[dmy_dash], format = "%d-%m-%Y"))
  remaining <- is.na(out) & !dmy_slash & !dmy_dash
  out[remaining] <- suppressWarnings(as.Date(substr(z[remaining], 1, 10), format = "%Y-%m-%d"))
  missing <- is.na(out)
  numeric_date <- missing & grepl("^\\d+(\\.\\d+)?$", z)
  out[numeric_date] <- as.Date(as.numeric(z[numeric_date]), origin = "1899-12-30")
  out
}
ym_seq <- function(start, end) {
  dates <- seq(as.Date(paste0(start, "-01")), as.Date(paste0(end, "-01")), by = "month")
  data.table(date = dates, year = as.integer(format(dates, "%Y")),
             month = as.integer(format(dates, "%m")), period = seq_along(dates))
}

calendar <- ym_seq(start_ym, end_ym)
if (!nrow(calendar)) stop("Janela temporal vazia.", call. = FALSE)

message("Lendo unidades pareadas")
units <- fread(matched_file, sep = ";", colClasses = c(cnpj_raiz = "character"))
units[, cnpj_raiz := clean_root(cnpj_raiz)]
units <- unique(units, by = c("unit_id", "cnpj_raiz", "coorte_match"))
if (!"match_weight" %in% names(units)) units[, match_weight := 1]
units[!is.finite(match_weight) | match_weight <= 0, match_weight := 1]

message("Lendo e normalizando estoques mensais")
monthly_raw <- as.data.table(read_excel(monthly_file, sheet = "emprego_mensal", col_types = "text"))
root_lengths <- monthly_raw[, .N, by = .(comprimento_original = nchar(clean_digits(cnpj_raiz)))]
monthly_raw[, cnpj_raiz := clean_root(cnpj_raiz)]
month_cols <- paste0("empregados_m", sprintf("%02d", 1:12))
for (v in month_cols) monthly_raw[, (v) := parse_num(get(v))]
monthly <- melt(monthly_raw, id.vars = c("cnpj_raiz", "ano"), measure.vars = month_cols,
                variable.name = "month_name", value.name = "emprego")
monthly[, `:=`(year = as.integer(ano), month = as.integer(sub("empregados_m", "", month_name)))]
monthly <- monthly[, .(emprego = sum(emprego, na.rm = TRUE)), by = .(cnpj_raiz, year, month)]
if (!file.exists(mass_file)) {
  stop("Base mensal de massa salarial ausente. Execute scripts/build_massa_salarial_mensal.R.", call. = FALSE)
}
mass <- fread(mass_file, sep = ";", colClasses = c(cnpj_raiz = "character"))
mass[, `:=`(cnpj_raiz = clean_root(cnpj_raiz), year = as.integer(ano), month = as.integer(mes))]
mass <- mass[, .(massa_salarial = sum(massa_salarial, na.rm = TRUE)), by = .(cnpj_raiz, year, month)]

message("Identificando a data exata do primeiro apoio FUST")
ops <- as.data.table(read_excel(operations_file, sheet = "base_compilada"))
ops[, cnpj_raiz := clean_root(cnpj)]
ops[, treatment_date := parse_date(data_da_contratacao)]
ops[, cliente_norm := normalize_name(cliente)]
panel_fust <- as.data.table(read_excel(fust_panel_file, sheet = "painel_cnpj_ano", col_types = "text"))
panel_fust[, `:=`(cnpj_raiz = clean_root(cnpj_raiz), cliente_norm = normalize_name(cliente_fust))]
name_map <- unique(panel_fust[!is.na(cnpj_raiz) & cliente_norm != "", .(cliente_norm, cnpj_raiz)])
dates_by_name <- ops[!is.na(treatment_date) & cliente_norm != "",
                     .(treatment_date = min(treatment_date)), by = cliente_norm]
dates_by_name <- merge(name_map, dates_by_name, by = "cliente_norm", all = FALSE)[, cliente_norm := NULL]
dates_by_root <- ops[!is.na(cnpj_raiz) & !is.na(treatment_date),
                     .(treatment_date = min(treatment_date)), by = cnpj_raiz]
first_treatment <- rbindlist(list(dates_by_root, dates_by_name))[
  , .(treatment_date = min(treatment_date)), by = cnpj_raiz
]

base <- CJ(unit_id = unique(units$unit_id), period = calendar$period, unique = TRUE)
base <- merge(base, units[, .(unit_id, cnpj_raiz, treat, coorte_match, match_weight)],
              by = "unit_id", all.x = TRUE)
base <- merge(base, calendar, by = "period", all.x = TRUE)
base <- merge(base, monthly, by = c("cnpj_raiz", "year", "month"), all.x = TRUE)
base <- merge(base, mass, by = c("cnpj_raiz", "year", "month"), all.x = TRUE)
base <- merge(base, first_treatment, by = "cnpj_raiz", all.x = TRUE)
base[is.na(emprego), emprego := 0]
base[is.na(massa_salarial), massa_salarial := 0]
base[, treatment_period := as.numeric(match(format(treatment_date, "%Y-%m"), format(calendar$date, "%Y-%m")))]
base[is.na(treatment_period) | treat == 0, treatment_period := 0]
message("Tratados com data na janela: ", uniqueN(base[treatment_period > 0L, unit_id]),
        "; coortes mensais: ", paste(sort(unique(base[treatment_period > 0L, treatment_period])), collapse = ", "))
base[, log_emprego := log1p(emprego)]
base[, log_massa_salarial := log1p(massa_salarial)]
setorder(base, unit_id, period)

# A unidade precisa ter desfecho em todos os períodos; zeros representam ausência
# de vínculos na RAIS para uma raiz que foi explicitamente pesquisada.
eligible_ids <- base[, .(ok = .N == nrow(calendar) &
                           all(is.finite(log_emprego)) &
                           all(is.finite(log_massa_salarial))), by = unit_id][ok == TRUE, unit_id]
base <- base[unit_id %in% eligible_ids]
base[, id_num := as.integer(factor(unit_id))]

make_quarterly <- function(dt) {
  out <- copy(dt)
  out[, quarter := (month - 1L) %/% 3L + 1L]
  out[, qperiod := match(paste(year, quarter),
                         unique(paste(calendar$year, (calendar$month - 1L) %/% 3L + 1L)))]
  out[, treatment_qperiod := as.numeric(fifelse(
    treatment_period > 0, (treatment_period - 1) %/% 3 + 1, 0
  ))]
  out[, .(
    cnpj_raiz = first(cnpj_raiz), treat = first(treat), coorte_match = first(coorte_match),
    match_weight = first(match_weight), emprego = mean(emprego),
    massa_salarial = mean(massa_salarial),
    log_emprego = log1p(mean(emprego)),
    log_massa_salarial = log1p(mean(massa_salarial)), g = first(treatment_qperiod),
    year = first(year), quarter = first(quarter)
  ), by = .(unit_id, id_num, period = qperiod)]
}

estimate_cs <- function(dt, frequency, outcome, outcome_label) {
  g_col <- if (frequency == "mensal") "treatment_period" else "g"
  fit <- att_gt(
    yname = outcome, tname = "period", idname = "id_num", gname = g_col,
    xformla = ~1, data = as.data.frame(dt), panel = TRUE,
    control_group = "notyettreated", anticipation = 0, base_period = "universal",
    bstrap = bootstrap_reps > 0,
    biters = max(bootstrap_reps, 1L),
    clustervars = if (bootstrap_reps > 0) "id_num" else NULL,
    print_details = FALSE
  )
  dynamic <- aggte(fit, type = "dynamic", na.rm = TRUE)
  simple <- aggte(fit, type = "simple", na.rm = TRUE)
  dyn <- data.table(
    frequency = frequency, outcome = outcome_label,
    event_time = dynamic$egt, att = dynamic$att.egt,
    se = dynamic$se.egt, ci_low = dynamic$att.egt - 1.96 * dynamic$se.egt,
    ci_high = dynamic$att.egt + 1.96 * dynamic$se.egt
  )
  summary <- data.table(
    frequency = frequency, outcome = outcome_label, estimator = "Callaway-Sant'Anna",
    estimate = simple$overall.att, se = simple$overall.se,
    ci_low = simple$overall.att - 1.96 * simple$overall.se,
    ci_high = simple$overall.att + 1.96 * simple$overall.se
  )
  list(dynamic = dyn, summary = summary)
}

estimate_sdid <- function(dt, frequency, outcome, outcome_label) {
  g_col <- if (frequency == "mensal") "treatment_period" else "g"
  cohorts <- sort(unique(dt[[g_col]][dt[[g_col]] > 1 & dt[[g_col]] <= max(dt$period)]))
  results <- rbindlist(lapply(cohorts, function(cohort) {
    cohort_dt <- copy(dt[get(g_col) %in% c(0L, cohort)])
    cohort_dt[, cohort_group := get(g_col)]
    wide <- dcast(cohort_dt, unit_id + cohort_group ~ period, value.var = outcome)
    setnames(wide, "cohort_group", "g")
    wide <- wide[complete.cases(wide)]
    setorder(wide, g, unit_id)
    n0 <- sum(wide$g == 0L)
    n1 <- sum(wide$g == cohort)
    t0 <- cohort - 1L
    if (n0 < 2L || n1 < 2L || t0 < 2L || t0 >= ncol(wide) - 2L) {
      return(data.table(frequency = frequency, outcome = outcome_label, cohort = cohort, n_treated = n1,
                        n_controls = n0, estimate = NA_real_, se = NA_real_, status = "amostra_insuficiente"))
    }
    y <- as.matrix(wide[, -c("unit_id", "g")])
    est <- tryCatch(synthdid_estimate(y, n0, t0), error = function(e) NULL)
    if (is.null(est)) return(data.table(frequency = frequency, outcome = outcome_label, cohort = cohort, n_treated = n1,
                                       n_controls = n0, estimate = NA_real_, se = NA_real_, status = "erro"))
    se <- tryCatch(synthdid_se(est, method = "placebo", replications = sdid_reps),
                   error = function(e) NA_real_)
    data.table(frequency = frequency, outcome = outcome_label, cohort = cohort, n_treated = n1, n_controls = n0,
               estimate = as.numeric(est), se = as.numeric(se), status = "estimado")
  }), fill = TRUE)
  valid <- results[status == "estimado" & is.finite(estimate)]
  if (nrow(valid)) {
    agg <- valid[, .(
      frequency = frequency[1], outcome = outcome[1], estimator = "SDID",
      estimate = weighted.mean(estimate, n_treated),
      se = sqrt(weighted.mean(se^2, n_treated, na.rm = TRUE))
    )]
    agg[, `:=`(ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se)]
  } else {
    agg <- data.table(frequency = frequency, outcome = outcome_label, estimator = "SDID", estimate = NA_real_,
                      se = NA_real_, ci_low = NA_real_, ci_high = NA_real_)
  }
  list(cohorts = results, summary = agg)
}

plot_event <- function(dt, frequency, outcome_slug, outcome_label) {
  p <- ggplot(dt, aes(event_time, att)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = .35) +
    geom_vline(xintercept = -1, color = "grey60", linetype = "dashed", linewidth = .35) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8FB8CA", alpha = .25) +
    geom_line(color = "#1C5D73", linewidth = .8) +
    geom_point(color = "#1C5D73", size = 1.8) +
    labs(title = paste("Callaway–Sant'Anna — frequência", frequency),
         subtitle = paste0(outcome_label, ", janela pós-Covid (2022–2024); baseline universal = -1"),
         x = paste("Períodos relativos ao primeiro apoio (", frequency, ")", sep = ""),
         y = paste0("ATT em log(1 + ", tolower(outcome_label), ")")) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, paste0("event_study_callaway_", frequency, "_", outcome_slug, ".png")),
         p, width = 8, height = 5, dpi = 300, bg = "white")
}

plot_sdid <- function(dt, frequency, outcome_slug, outcome_label) {
  plot_dt <- dt[status == "estimado" & is.finite(estimate)]
  if (!nrow(plot_dt)) return(invisible(NULL))
  plot_dt[, `:=`(ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se)]
  p <- ggplot(plot_dt, aes(factor(cohort), estimate)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = .35) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .12, color = "#1C5D73") +
    geom_point(size = 2.4, color = "#1C5D73") +
    labs(title = paste("SDID por coorte — frequência", frequency),
         subtitle = paste0(outcome_label, ", janela pós-Covid (2022–2024)"),
         x = "Período da primeira contratação", y = "ATT SDID") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(), plot.title = element_text(face = "bold"))
  ggsave(file.path(out_dir, paste0("sdid_por_coorte_", frequency, "_", outcome_slug, ".png")),
         p, width = 8, height = 5, dpi = 300, bg = "white")
}

set.seed(seed)
quarterly <- make_quarterly(base)
outcomes <- data.table(
  variable = c("log_emprego", "log_massa_salarial"),
  slug = c("emprego", "massa_salarial"),
  label = c("Emprego", "Massa salarial")
)
results <- list()
for (i in seq_len(nrow(outcomes))) {
  variable <- outcomes$variable[i]
  slug_out <- outcomes$slug[i]
  label_out <- outcomes$label[i]
  cs_month <- estimate_cs(base, "mensal", variable, label_out)
  cs_quarter <- estimate_cs(quarterly, "trimestral", variable, label_out)
  sdid_month <- estimate_sdid(base, "mensal", variable, label_out)
  sdid_quarter <- estimate_sdid(quarterly, "trimestral", variable, label_out)
  plot_event(cs_month$dynamic, "mensal", slug_out, label_out)
  plot_event(cs_quarter$dynamic, "trimestral", slug_out, label_out)
  plot_sdid(sdid_month$cohorts, "mensal", slug_out, label_out)
  plot_sdid(sdid_quarter$cohorts, "trimestral", slug_out, label_out)
  results[[slug_out]] <- list(
    cs_month = cs_month, cs_quarter = cs_quarter,
    sdid_month = sdid_month, sdid_quarter = sdid_quarter
  )
}
summary_results <- rbindlist(lapply(results, function(x) rbindlist(list(
  x$cs_month$summary, x$cs_quarter$summary, x$sdid_month$summary, x$sdid_quarter$summary
), fill = TRUE)), fill = TRUE)
cs_month_all <- rbindlist(lapply(results, function(x) x$cs_month$dynamic), fill = TRUE)
cs_quarter_all <- rbindlist(lapply(results, function(x) x$cs_quarter$dynamic), fill = TRUE)
sdid_month_all <- rbindlist(lapply(results, function(x) x$sdid_month$cohorts), fill = TRUE)
sdid_quarter_all <- rbindlist(lapply(results, function(x) x$sdid_quarter$cohorts), fill = TRUE)
coverage <- data.table(
  inicio = start_ym, fim = end_ym, unidades_pareadas = uniqueN(units$unit_id),
  unidades_painel = uniqueN(base$unit_id), tratados_na_janela = uniqueN(base[treatment_period > 0, unit_id]),
  controles_ou_tratados_futuros = uniqueN(base[treatment_period == 0, unit_id]),
  linhas_mensais = nrow(base), linhas_trimestrais = nrow(quarterly),
  bootstrap_callaway = bootstrap_reps, replicacoes_placebo_sdid = sdid_reps,
  observacao = "CNPJ raiz normalizado para os 8 primeiros dígitos; ausências na RAIS mensal preenchidas com estoque zero."
)

fwrite(base, file.path(out_dir, "painel_mensal.csv"), sep = ";", bom = TRUE, na = "")
fwrite(quarterly, file.path(out_dir, "painel_trimestral.csv"), sep = ";", bom = TRUE, na = "")
fwrite(cs_month_all, file.path(out_dir, "event_study_callaway_mensal.csv"), sep = ";", bom = TRUE, na = "")
fwrite(cs_quarter_all, file.path(out_dir, "event_study_callaway_trimestral.csv"), sep = ";", bom = TRUE, na = "")
fwrite(sdid_month_all, file.path(out_dir, "sdid_por_coorte_mensal.csv"), sep = ";", bom = TRUE, na = "")
fwrite(sdid_quarter_all, file.path(out_dir, "sdid_por_coorte_trimestral.csv"), sep = ";", bom = TRUE, na = "")
fwrite(summary_results, file.path(out_dir, "resumo_resultados.csv"), sep = ";", bom = TRUE, na = "")
fwrite(coverage, file.path(out_dir, "auditoria_cobertura.csv"), sep = ";", bom = TRUE, na = "")
fwrite(root_lengths, file.path(out_dir, "auditoria_comprimento_cnpj_original.csv"), sep = ";", bom = TRUE, na = "")
write_xlsx(list(resumo = summary_results, cobertura = coverage,
                callaway_mensal = cs_month_all, callaway_trimestral = cs_quarter_all,
                sdid_mensal = sdid_month_all, sdid_trimestral = sdid_quarter_all),
           file.path(out_dir, "resultados_did_frequencias.xlsx"))
message("Concluído: ", out_dir)
