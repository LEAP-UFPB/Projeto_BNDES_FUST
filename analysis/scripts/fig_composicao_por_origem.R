args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

plotar_composicao_origem <- function(nome_arquivo, contratos_unicos = FALSE) {
  base <- base_2024_2025()

  df <- if (contratos_unicos) {
    contar_contratos_unicos(base, ano, origem, name = "contratos")
  } else {
    base |>
      count(ano, origem, name = "contratos")
  }

  df <- df |>
    complete(ano = c(2024L, 2025L), origem, fill = list(contratos = 0)) |>
    group_by(ano) |>
    mutate(
      frac = contratos / sum(contratos),
      label = if_else(contratos > 0, paste0(fmt_int(contratos), "\n", percent(frac, accuracy = 0.1, decimal.mark = ",")), "")
    ) |>
    ungroup() |>
    mutate(ano = factor(ano, levels = c(2024L, 2025L)))

  p <- ggplot(df, aes(x = 2, y = frac, fill = origem)) +
    geom_col(width = 1, color = cor_fundo, linewidth = 0.8) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 3.4,
      lineheight = 0.95
    ) +
    coord_polar(theta = "y") +
    xlim(0.5, 2.5) +
    facet_wrap(~ano, nrow = 1, scales = "fixed") +
    scale_fill_manual(values = paleta_origem, drop = FALSE) +
    labs(title = "Composicao por origem", fill = NULL) +
    tema_donut_escuro()

  salvar_figura_escura(p, nome_arquivo, width = 7, height = 4.5)
}

plotar_composicao_origem("fig_composicao_por_origem.png")
plotar_composicao_origem("fig_composicao_por_origem_contratos_unicos.png", contratos_unicos = TRUE)
