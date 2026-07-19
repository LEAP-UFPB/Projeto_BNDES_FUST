args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

plotar_porte <- function(metrica, titulo, nome_arquivo, dinheiro = FALSE) {
  base <- base_2024_2025()

  resumo <- if (metrica == "contratos_unicos") {
    contar_contratos_unicos(base, ano, porte_do_cliente, origem, name = "valor")
  } else if (metrica == "contratos") {
    base |>
      count(ano, porte_do_cliente, origem, name = "valor")
  } else {
    base |>
      group_by(ano, porte_do_cliente, origem) |>
      summarise(valor = sum(.data[[metrica]], na.rm = TRUE), .groups = "drop")
  }

  df <- resumo |>
    complete(
      ano = c(2024L, 2025L),
      porte_do_cliente,
      origem = factor(c("Direta", "Indireta"), levels = c("Direta", "Indireta")),
      fill = list(valor = 0)
    ) |>
    group_by(ano, porte_do_cliente) |>
    mutate(total_porte = sum(valor, na.rm = TRUE)) |>
    ungroup() |>
    filter(normalizar_texto(porte_do_cliente) != "NAO INFORMADO" | total_porte > 0)

  porte_levels <- rev(levels(df$porte_do_cliente))
  porte_levels <- porte_levels[porte_levels %in% as.character(df$porte_do_cliente)]

  df <- df |>
    mutate(
      ano = factor(ano, levels = c(2025L, 2024L)),
      porte_do_cliente = factor(as.character(porte_do_cliente), levels = porte_levels),
      label_total = case_when(
        valor <= 0 ~ "",
        dinheiro ~ fmt_dinheiro_curto(valor),
        TRUE ~ fmt_int(valor)
      ),
      ano_label = as.character(ano)
    )
  eixo_max <- max(df$valor, na.rm = TRUE)
  df <- df |>
    mutate(ano_pos = -0.055 * eixo_max)

  label_eixo <- if (dinheiro) {
    function(x) if_else(x < 0, "", paste0(number(x / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 1), " mi"))
  } else {
    function(x) if_else(x < 0, "", label_number(big.mark = ".", decimal.mark = ",")(x))
  }

  p <- ggplot(df, aes(x = porte_do_cliente, y = valor, fill = ano)) +
    geom_col(position = position_dodge(width = 0.74), width = 0.64) +
    geom_text(
      aes(y = ano_pos, label = ano_label),
      position = position_dodge(width = 0.74),
      color = "black",
      fontface = "bold",
      size = 2.8
    ) +
    geom_text(
      aes(label = label_total),
      position = position_dodge(width = 0.74),
      hjust = -0.12,
      color = "black",
      fontface = "bold",
      size = 3
    ) +
    facet_wrap(~origem, nrow = 1, scales = "fixed") +
    coord_flip(clip = "off") +
    scale_fill_manual(values = c("2024" = cor_azul, "2025" = cor_verde), breaks = c("2024", "2025"), drop = FALSE) +
    scale_y_continuous(
      limits = c(-0.09 * eixo_max, NA),
      labels = label_eixo,
      expand = expansion(mult = c(0, 0.28))
    ) +
    labs(title = titulo, x = NULL, y = NULL) +
    tema_fust_escuro() +
    theme(
      axis.text.x = element_text(color = "black", face = "bold", size = 9),
      panel.spacing.x = unit(1.6, "lines"),
      plot.margin = margin(18, 80, 22, 34)
    )

  salvar_figura_escura(p, nome_arquivo, width = 10.5, height = 6.2)
}

plotar_porte(
  "contratos",
  "Numero de operacoes por porte do cliente",
  "fig_composicao_por_porte.png",
  dinheiro = FALSE
)

plotar_porte(
  "contratos_unicos",
  "Numero de contratos unicos por porte do cliente",
  "fig_composicao_por_porte_contratos_unicos.png",
  dinheiro = FALSE
)

plotar_porte(
  "valor_contratado",
  "Valor contratado por porte do cliente",
  "fig_composicao_por_porte_valor_contratado.png",
  dinheiro = TRUE
)

plotar_porte(
  "valor_desembolsado",
  "Valor desembolsado por porte do cliente",
  "fig_composicao_por_porte_valor_desembolsado.png",
  dinheiro = TRUE
)
