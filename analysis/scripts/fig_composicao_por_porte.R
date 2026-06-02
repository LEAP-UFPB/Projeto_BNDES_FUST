args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  count(porte_do_cliente, name = "contratos") |>
  filter(contratos > 0) |>
  arrange(porte_do_cliente)

p <- ggplot(df, aes(x = porte_do_cliente, y = contratos)) +
  geom_col(fill = cor_verde, color = "black", linewidth = 0.35, width = 0.68) +
  geom_text(aes(label = fmt_int(contratos)), hjust = -0.12, size = 3.6) +
  coord_flip(clip = "off") +
  scale_x_discrete(limits = rev(as.character(df$porte_do_cliente))) +
  scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ","), expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Contratos por porte do cliente", x = NULL, y = "Contratos") +
  tema_fust() +
  theme(plot.margin = margin(10, 28, 10, 10))

salvar_figura(p, "fig_composicao_por_porte.png", width = 8, height = 5)

