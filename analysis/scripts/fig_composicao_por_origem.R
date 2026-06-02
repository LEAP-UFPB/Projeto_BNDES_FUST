args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  count(origem_detalhada, name = "contratos") |>
  arrange(desc(contratos)) |>
  mutate(
    frac = contratos / sum(contratos),
    label = paste0(origem_detalhada, " - ", percent(frac, accuracy = 0.1, decimal.mark = ","))
  )

png(
  filename = file.path(output_dir, "fig_composicao_por_origem.png"),
  width = 2000,
  height = 1350,
  res = 220,
  bg = "white"
)

layout(matrix(c(1, 2), nrow = 2), heights = c(5.3, 1))

par(mar = c(1, 5, 1, 5), family = "sans", xpd = NA)
pie(
  x = df$contratos,
  labels = df$label,
  col = paleta_fust[seq_len(nrow(df))],
  border = "black",
  clockwise = TRUE,
  init.angle = 90,
  radius = 0.72,
  cex = 1.15
)

par(mar = c(0, 1, 1, 1), family = "sans")
plot.new()
legend(
  "center",
  title = "Origem",
  legend = df$origem_detalhada,
  fill = paleta_fust[seq_len(nrow(df))],
  border = "black",
  horiz = TRUE,
  bty = "n",
  cex = 1.1
)

dev.off()
