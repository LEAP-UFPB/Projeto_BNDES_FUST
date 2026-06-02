args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  group_by(ano) |>
  summarise(valor_contratado = sum(valor_contratado, na.rm = TRUE), .groups = "drop")

p <- ggplot(df, aes(x = factor(ano), y = valor_contratado)) +
  geom_col(fill = cor_verde, color = "black", linewidth = 0.35, width = 0.58) +
  geom_text(aes(label = fmt_dinheiro_curto(valor_contratado)), vjust = -0.35, size = 4) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " mi", big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Valor contratado por ano", x = NULL, y = "Valor contratado") +
  tema_fust() +
  theme(plot.margin = margin(18, 10, 10, 10))

salvar_figura(p, "fig_valor_contratado_por_ano.png", width = 7, height = 4.5)

