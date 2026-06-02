args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  count(ano, name = "contratos")

p <- ggplot(df, aes(x = factor(ano), y = contratos)) +
  geom_col(fill = cor_azul, color = "black", linewidth = 0.35, width = 0.58) +
  geom_text(aes(label = fmt_int(contratos)), vjust = -0.35, size = 4) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Contratos por ano", x = NULL, y = "Contratos") +
  tema_fust() +
  theme(plot.margin = margin(18, 10, 10, 10))

salvar_figura(p, "fig_contratos_por_ano.png", width = 7, height = 4.5)

