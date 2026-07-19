#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args_all <- commandArgs(trailingOnly = FALSE)
script_arg <- args_all[grepl("^--file=", args_all)]
script_path <- normalizePath(sub("^--file=", "", script_arg[1]), winslash = "/")
out_dir <- normalizePath(file.path(dirname(script_path), "..", "outputs"), winslash = "/")

slug <- function(x) {
  ifelse(x == "Log emprego", "log_emprego", "log_massa_salarial")
}

notyet <- fread(file.path(out_dir, "att_event_study_not_yet_treated.csv"), sep = ";")
for (label in unique(notyet$outcome)) {
  dt <- notyet[outcome == label]
  plot <- ggplot(dt, aes(event_time, att)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    geom_vline(xintercept = -1, color = "grey60", linetype = "dashed", linewidth = 0.35) +
    geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#8FB8CA", alpha = 0.25) +
    geom_line(color = "#1C5D73", linewidth = 0.8) +
    geom_point(color = "#1C5D73", size = 2.2) +
    scale_x_continuous(breaks = sort(unique(dt$event_time))) +
    labs(
      title = paste0("Event study not-yet-treated — ", label),
      x = "Tempo relativo ao primeiro apoio FUST",
      y = "ATT"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
  ggsave(
    file.path(out_dir, paste0("event_study_not_yet_treated_", slug(label), ".png")),
    plot, width = 8, height = 5, dpi = 300, bg = "white"
  )
}

sdid <- fread(file.path(out_dir, "synthetic_did_por_coorte.csv"), sep = ";")
sdid <- sdid[status == "estimado"]
for (label in unique(sdid$outcome)) {
  dt <- sdid[outcome == label]
  plot_dt <- rbindlist(list(
    dt[, .(coorte, estimativa = att, se, tipo = "ATT pós-tratamento")],
    dt[, .(coorte, estimativa = placebo_pre, se = placebo_se, tipo = "Placebo pré-tratamento")]
  ))
  plot_dt[, `:=`(ci_low = estimativa - 1.96 * se, ci_high = estimativa + 1.96 * se)]
  plot <- ggplot(plot_dt, aes(factor(coorte), estimativa, color = tipo)) +
    geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.12, position = position_dodge(width = 0.35)) +
    geom_point(size = 2.5, position = position_dodge(width = 0.35)) +
    scale_color_manual(values = c("ATT pós-tratamento" = "#1C5D73", "Placebo pré-tratamento" = "#B35C44")) +
    labs(
      title = paste0("Synthetic DID e placebo — ", label),
      x = "Coorte de primeiro apoio FUST",
      y = "Estimativa",
      color = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank(), legend.position = "bottom")
  ggsave(
    file.path(out_dir, paste0("synthetic_did_placebo_", slug(label), ".png")),
    plot, width = 8, height = 5, dpi = 300, bg = "white"
  )
}
