args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  group_by(ano) |>
  summarise(
    valor_operacao = sum(valor_operacao, na.rm = TRUE),
    valor_desembolsado = sum(valor_desembolsado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(valor_operacao, valor_desembolsado),
    names_to = "medida",
    values_to = "valor"
  ) |>
  mutate(
    medida = recode(
      medida,
      valor_operacao = "Valor da operação",
      valor_desembolsado = "Valor desembolsado"
    )
  )

p <- ggplot(df, aes(x = factor(ano), y = valor, fill = medida)) +
  geom_col(position = position_dodge(width = 0.72), color = "black", linewidth = 0.35, width = 0.62) +
  geom_text(
    aes(label = fmt_dinheiro_curto(valor)),
    position = position_dodge(width = 0.72),
    vjust = -0.35,
    size = 3.3
  ) +
  scale_fill_manual(values = c("Valor da operação" = cor_azul, "Valor desembolsado" = cor_verde)) +
  scale_y_continuous(
    labels = label_number(scale = 1e-6, suffix = " mi", big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "Valor da operação versus valor desembolsado",
    subtitle = "Comparativo por ano",
    x = NULL,
    y = "Valor",
    fill = "Medida"
  ) +
  tema_fust() +
  theme(plot.margin = margin(18, 10, 10, 10))

salvar_figura(p, "fig_operacao_vs_desembolso_por_ano.png", width = 8, height = 5)
