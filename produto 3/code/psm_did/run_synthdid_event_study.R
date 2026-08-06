#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(synthdid)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(name, default) {
  prefix <- paste0("--", name, "=")
  hit <- args[startsWith(args, prefix)]
  if (length(hit)) sub(prefix, "", hit[1], fixed = TRUE) else default
}

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- args_all[grepl("^--file=", args_all)]
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/")
out_dir <- normalizePath(file.path(dirname(script_path), "..", "..", "analysis", "psm_did"), winslash = "/")
bootstrap_reps <- as.integer(arg_value("bootstrap-reps", "50"))
bootstrap_seed <- as.integer(arg_value("bootstrap-seed", "3854035"))

units <- fread(file.path(out_dir, "unidades_pareadas.csv"), sep = ";", colClasses = c(cnpj_raiz = "character"))
panel_cache <- fread(
  file.path(out_dir, "painel_did_psm_rais_2021_2025_v2.csv"),
  sep = ";", colClasses = c(cnpj_raiz = "character")
)
outcomes <- unique(
  panel_cache[, .(cnpj_raiz, ano, log_emprego, log_massa_salarial)],
  by = c("cnpj_raiz", "ano")
)
panel <- merge(outcomes, units, by = "cnpj_raiz", allow.cartesian = TRUE)

estimate_cohort_curve <- function(dt, outcome, cohort) {
  cohort_dt <- unique(
    dt[coorte_match == cohort, .(unit_id, ano, treat, y = get(outcome))],
    by = c("unit_id", "ano")
  )
  wide <- dcast(cohort_dt, unit_id + treat ~ ano, value.var = "y")
  wide <- wide[complete.cases(wide)]
  setorder(wide, treat, unit_id)
  if (sum(wide$treat == 1) < 2 || sum(wide$treat == 0) < 2) return(NULL)

  years <- as.integer(setdiff(names(wide), c("unit_id", "treat")))
  year_cols <- as.character(years)
  Y <- as.matrix(wide[, ..year_cols])
  N0 <- sum(wide$treat == 0)
  T0 <- sum(years < cohort)
  estimate <- synthdid_estimate(Y, N0, T0)
  weights <- attr(estimate, "weights")
  setup <- attr(estimate, "setup")
  N1 <- nrow(setup$Y) - setup$N0
  gap <- as.numeric(t(c(-weights$omega, rep(1 / N1, N1))) %*% setup$Y)
  baseline <- as.numeric(gap[seq_len(setup$T0)] %*% weights$lambda)
  data.table(
    coorte = cohort,
    event_time = as.integer(colnames(setup$Y)) - cohort,
    estimate = gap - baseline,
    cohort_weight = N1,
    cohort_att = as.numeric(estimate)
  )
}

estimate_event_curve <- function(dt, outcome) {
  curves <- rbindlist(lapply(sort(unique(dt$coorte_match)), function(cohort) {
    tryCatch(estimate_cohort_curve(dt, outcome, cohort), error = function(e) NULL)
  }), fill = TRUE)
  curves <- curves[event_time != -1]
  aggregate <- curves[, .(
    estimate = weighted.mean(estimate, cohort_weight),
    n_cohorts = .N,
    treated_units = sum(cohort_weight)
  ), by = event_time]
  rbind(
    aggregate,
    data.table(event_time = -1L, estimate = 0, n_cohorts = NA_integer_, treated_units = NA_real_),
    fill = TRUE
  )[order(event_time)]
}

resample_panel <- function(dt) {
  rbindlist(lapply(sort(unique(dt$coorte_match)), function(cohort) {
    cohort_dt <- dt[coorte_match == cohort]
    rbindlist(lapply(c(0L, 1L), function(role) {
      role_dt <- cohort_dt[treat == role]
      ids <- unique(role_dt$unit_id)
      sampled <- sample(ids, length(ids), replace = TRUE)
      rbindlist(lapply(seq_along(sampled), function(index) {
        copy(role_dt[unit_id == sampled[index]])[, unit_id := paste(cohort, role, index, sep = "_")]
      }))
    }))
  }))
}

bootstrap_event <- function(dt, outcome, event_times) {
  set.seed(bootstrap_seed)
  draws <- matrix(NA_real_, nrow = bootstrap_reps, ncol = length(event_times))
  colnames(draws) <- as.character(event_times)
  for (rep in seq_len(bootstrap_reps)) {
    estimate <- tryCatch(estimate_event_curve(resample_panel(dt), outcome), error = function(e) NULL)
    if (!is.null(estimate)) {
      positions <- match(event_times, estimate$event_time)
      draws[rep, !is.na(positions)] <- estimate$estimate[positions[!is.na(positions)]]
    }
  }
  apply(draws, 2, sd, na.rm = TRUE)
}

slug <- function(outcome) ifelse(outcome == "log_emprego", "log_emprego", "log_massa_salarial")
labels <- c(log_emprego = "Log emprego", log_massa_salarial = "Log massa salarial")
all_results <- list()

for (outcome in names(labels)) {
  estimate <- estimate_event_curve(panel, outcome)
  estimate[, se := bootstrap_event(panel, outcome, event_time)]
  estimate[event_time == -1, se := 0]
  estimate[, `:=`(ci_low = estimate - 1.96 * se, ci_high = estimate + 1.96 * se)]
  estimate[, outcome := labels[[outcome]]]
  all_results[[outcome]] <- estimate

  plot <- ggplot(estimate, aes(event_time, estimate)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -1, color = "grey60", linetype = "dashed", linewidth = 0.35) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8FB8CA", alpha = 0.25) +
    geom_line(color = "#1C5D73", linewidth = 0.8) +
    geom_point(color = "#1C5D73", size = 2.2) +
    scale_x_continuous(breaks = sort(unique(estimate$event_time))) +
    labs(
      title = paste0("Synthetic DID event study — ", labels[[outcome]]),
      x = "Tempo relativo ao primeiro apoio FUST",
      y = "Efeito SDID"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  ggsave(
    file.path(out_dir, paste0("event_study_synthetic_did_", slug(outcome), ".png")),
    plot, width = 8, height = 5, dpi = 300, bg = "white"
  )
}

fwrite(
  rbindlist(all_results, fill = TRUE),
  file.path(out_dir, "event_study_synthetic_did.csv"),
  sep = ";", bom = TRUE, na = ""
)
