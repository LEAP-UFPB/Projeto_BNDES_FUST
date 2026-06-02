args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  filter(!is.na(uf)) |>
  group_by(uf) |>
  summarise(valor_contratado = sum(valor_contratado, na.rm = TRUE), .groups = "drop") |>
  slice_max(valor_contratado, n = 10)

p <- ggplot(df, aes(x = reorder(uf, valor_contratado), y = valor_contratado)) +
  geom_col(fill = cor_azul, color = "black", linewidth = 0.35, width = 0.68) +
  geom_text(aes(label = fmt_dinheiro_curto(valor_contratado)), hjust = -0.12, size = 3.6) +
  coord_flip(clip = "off") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " mi", big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.28))) +
  labs(title = "Top UFs por valor contratado", subtitle = "Contratos de 2024 e 2025", x = NULL, y = "Valor contratado") +
  tema_fust() +
  theme(plot.margin = margin(10, 34, 10, 10))

salvar_figura(p, "fig_top_uf_valor_contratado.png", width = 9, height = 5.5)

