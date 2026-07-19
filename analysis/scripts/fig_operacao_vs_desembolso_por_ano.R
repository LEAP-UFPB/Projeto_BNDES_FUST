args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  group_by(ano, origem) |>
  summarise(valor_desembolsado = sum(valor_desembolsado, na.rm = TRUE), .groups = "drop") |>
  complete(ano, origem, fill = list(valor_desembolsado = 0)) |>
  group_by(ano) |>
  mutate(label = if_else(valor_desembolsado > 0, fmt_dinheiro_curto(valor_desembolsado), "")) |>
  ungroup()

p <- ggplot(df, aes(x = factor(ano), y = valor_desembolsado, fill = origem)) +
  geom_col(width = 0.58) +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 3.8
  ) +
  scale_fill_manual(values = paleta_origem, drop = FALSE) +
  scale_y_continuous(
    labels = fmt_milhoes_eixo,
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  labs(title = "Valor desembolsado por ano", x = NULL, y = NULL) +
  tema_fust_escuro()

salvar_figura_escura(p, "fig_operacao_vs_desembolso_por_ano.png", width = 7, height = 4.5)
