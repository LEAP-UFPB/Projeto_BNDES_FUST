args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

plotar_contratos_por_ano <- function(nome_arquivo, contratos_unicos = FALSE) {
  base <- base_2024_2025()

  df <- if (contratos_unicos) {
    contar_contratos_unicos(base, ano, origem, name = "contratos")
  } else {
    base |>
      count(ano, origem, name = "contratos")
  }

  df <- df |>
    complete(ano, origem, fill = list(contratos = 0)) |>
    group_by(ano) |>
    mutate(label = if_else(contratos > 0, fmt_int(contratos), "")) |>
    ungroup()

  p <- ggplot(df, aes(x = factor(ano), y = contratos, fill = origem)) +
    geom_col(width = 0.58) +
    geom_text(
      aes(label = label),
      position = position_stack(vjust = 0.5),
      color = "black",
      fontface = "bold",
      size = 4
    ) +
    scale_fill_manual(values = paleta_origem, drop = FALSE) +
    scale_y_continuous(
      labels = label_number(big.mark = ".", decimal.mark = ","),
      expand = expansion(mult = c(0.04, 0.06))
    ) +
    labs(title = "Numero de operacoes por ano", x = NULL, y = NULL) +
    tema_fust_escuro()

  salvar_figura_escura(p, nome_arquivo, width = 7, height = 4.5)
}

plotar_contratos_por_ano("fig_contratos_por_ano.png")
plotar_contratos_por_ano("fig_contratos_unicos_por_ano.png", contratos_unicos = TRUE)

