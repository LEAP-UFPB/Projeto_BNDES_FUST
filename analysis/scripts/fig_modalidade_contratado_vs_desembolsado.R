args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025() |>
  group_by(ano, origem) |>
  summarise(
    contratado = sum(valor_contratado, na.rm = TRUE),
    desembolsado = sum(valor_desembolsado, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(contratado, desembolsado),
    names_to = "medida",
    values_to = "valor"
  ) |>
  mutate(
    medida = recode(
      medida,
      contratado = "Contratado",
      desembolsado = "Desembolsado"
    ),
    ano = factor(ano, levels = c(2025L, 2024L)),
    label = if_else(valor > 0, fmt_dinheiro_curto(valor), "")
  )

p <- ggplot(df, aes(x = ano, y = valor, fill = medida)) +
  geom_col(
    position = position_dodge(width = 0.72),
    width = 0.62
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.72),
    hjust = -0.12,
    color = "black",
    fontface = "bold",
    size = 3.1
  ) +
  facet_wrap(~origem, nrow = 1, scales = "fixed") +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("Contratado" = cor_azul, "Desembolsado" = cor_verde)
  ) +
  scale_y_continuous(
    labels = fmt_milhoes_eixo,
    expand = expansion(mult = c(0, 0.22))
  ) +
  labs(
    title = "Contratado versus desembolsado por modalidade",
    x = NULL,
    y = NULL
  ) +
  tema_fust_escuro() +
  theme(plot.margin = margin(18, 22, 18, 18))

salvar_figura_escura(p, "fig_modalidade_contratado_vs_desembolsado.png", width = 9, height = 4.8)
