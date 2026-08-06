args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

plotar_uf <- function(
  metrica,
  titulo,
  nome_arquivo,
  dinheiro = TRUE,
  top_n = 10,
  todas_ufs = FALSE
) {
  base <- base_2024_2025() |>
    filter(!is.na(uf))

  resumo <- if (metrica == "contratos_unicos") {
    contar_contratos_unicos(base, ano, uf, origem, name = "valor")
  } else if (metrica == "contratos") {
    base |>
      count(ano, uf, origem, name = "valor")
  } else {
    base |>
      group_by(ano, uf, origem) |>
      summarise(valor = sum(.data[[metrica]], na.rm = TRUE), .groups = "drop")
  }

  ranking_ufs <- resumo |>
    group_by(uf) |>
    summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
    arrange(total)

  ufs_plot <- if (todas_ufs) {
    ranking_ufs |>
      filter(total > 0) |>
      arrange(total) |>
      pull(uf)
  } else {
    ranking_ufs |>
      slice_max(total, n = top_n, with_ties = FALSE) |>
      arrange(total) |>
      pull(uf)
  }

  df_base <- resumo |>
    filter(uf %in% ufs_plot)

  df <- if (todas_ufs) {
    df_base |>
      filter(valor > 0)
  } else {
    df_base |>
      complete(
        ano = c(2024L, 2025L),
        uf = ufs_plot,
        origem = factor(c("Direta", "Indireta"), levels = c("Direta", "Indireta")),
        fill = list(valor = 0)
      )
  }

  df <- df |>
    mutate(
      ano = factor(ano, levels = c(2025L, 2024L)),
      origem = factor(as.character(origem), levels = c("Direta", "Indireta")),
      uf = factor(uf, levels = ufs_plot),
      label_total = case_when(
        valor <= 0 ~ "",
        dinheiro ~ fmt_dinheiro_curto(valor),
        TRUE ~ fmt_int(valor)
      ),
      ano_label = if_else(valor > 0, as.character(ano), "")
    )

  eixo_max <- max(df$valor, na.rm = TRUE)
  if (!is.finite(eixo_max) || eixo_max <= 0) {
    eixo_max <- 1
  }

  df <- df |>
    mutate(ano_pos = -0.055 * eixo_max)

  label_eixo <- if (dinheiro) {
    function(x) if_else(x < 0, "", paste0(number(x / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 1), " mi"))
  } else {
    function(x) if_else(x < 0, "", label_number(big.mark = ".", decimal.mark = ",")(x))
  }

  tamanho_rotulo <- if (todas_ufs) 2.1 else 3
  tamanho_ano <- if (todas_ufs) 2.1 else 2.8
  altura <- if (todas_ufs) 12.5 else 6.8

  p <- ggplot(df, aes(x = uf, y = valor, fill = ano)) +
    geom_col(position = position_dodge(width = 0.74), width = 0.64) +
    geom_text(
      aes(y = ano_pos, label = ano_label),
      position = position_dodge(width = 0.74),
      color = "black",
      fontface = "bold",
      size = tamanho_ano
    ) +
    geom_text(
      aes(label = label_total),
      position = position_dodge(width = 0.74),
      hjust = -0.12,
      color = "black",
      fontface = "bold",
      size = tamanho_rotulo
    ) +
    facet_wrap(~origem, nrow = 1, scales = "fixed") +
    coord_flip(clip = "off") +
    scale_fill_manual(
      values = c("2024" = cor_azul, "2025" = cor_verde),
      breaks = c("2024", "2025"),
      drop = FALSE
    ) +
    scale_y_continuous(
      limits = c(-0.09 * eixo_max, NA),
      labels = label_eixo,
      expand = expansion(mult = c(0, 0.28))
    ) +
    labs(title = titulo, x = NULL, y = NULL) +
    tema_fust_escuro() +
    theme(
      axis.text.x = element_text(color = "black", face = "bold", size = 9),
      axis.text.y = element_text(color = "black", face = "bold", size = if (todas_ufs) 8.5 else 12),
      panel.grid.major.y = element_blank(),
      panel.spacing.x = unit(1.6, "lines"),
      plot.margin = margin(18, 80, 22, 34)
    )

  salvar_figura_escura(p, nome_arquivo, width = 10.5, height = altura)
}

plotar_uf(
  "contratos",
  "Numero de operacoes por UF - top 10",
  "fig_top_uf_operacoes.png",
  dinheiro = FALSE
)

plotar_uf(
  "contratos_unicos",
  "Numero de contratos unicos por UF - top 10",
  "fig_top_uf_contratos_unicos.png",
  dinheiro = FALSE
)

plotar_uf(
  "valor_contratado",
  "Valor contratado por UF - top 10",
  "fig_top_uf_valor_contratado.png"
)

plotar_uf(
  "valor_desembolsado",
  "Valor desembolsado por UF - top 10",
  "fig_top_uf_valor_desembolsado.png"
)

plotar_uf(
  "contratos",
  "Numero de operacoes por UF - todas as UFs",
  "fig_uf_todas_operacoes.png",
  dinheiro = FALSE,
  todas_ufs = TRUE
)

plotar_uf(
  "contratos_unicos",
  "Numero de contratos unicos por UF - todas as UFs",
  "fig_uf_todas_contratos_unicos.png",
  dinheiro = FALSE,
  todas_ufs = TRUE
)

plotar_uf(
  "valor_contratado",
  "Valor contratado por UF - todas as UFs",
  "fig_uf_todas_valor_contratado.png",
  todas_ufs = TRUE
)

plotar_uf(
  "valor_desembolsado",
  "Valor desembolsado por UF - todas as UFs",
  "fig_uf_todas_valor_desembolsado.png",
  todas_ufs = TRUE
)
