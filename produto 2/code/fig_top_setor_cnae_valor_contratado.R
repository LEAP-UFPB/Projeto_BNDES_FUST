args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

plotar_setor <- function(metrica, titulo, nome_arquivo, dinheiro = TRUE) {
  base <- base_2024_2025()
  dimensao <- "subsetor_cnae"

  resumo <- if (metrica == "contratos_unicos") {
    contar_contratos_unicos(base, ano, .data[[dimensao]], origem, name = "valor")
  } else if (metrica == "contratos") {
    base |>
      count(ano, .data[[dimensao]], origem, name = "valor")
  } else {
    base |>
      group_by(ano, .data[[dimensao]], origem) |>
      summarise(valor = sum(.data[[metrica]], na.rm = TRUE), .groups = "drop")
  }

  top_setores <- resumo |>
    group_by(.data[[dimensao]]) |>
    summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") |>
    slice_max(total, n = 10) |>
    arrange(total) |>
    pull(.data[[dimensao]])

  df <- resumo |>
    filter(.data[[dimensao]] %in% top_setores) |>
    complete(
      ano = c(2024L, 2025L),
      subsetor_cnae = top_setores,
      origem = factor(c("Direta", "Indireta"), levels = c("Direta", "Indireta")),
      fill = list(valor = 0)
    ) |>
    mutate(
      ano = factor(ano, levels = c(2025L, 2024L)),
      subsetor_cnae = factor(subsetor_cnae, levels = top_setores),
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

  p <- ggplot(df, aes(x = subsetor_cnae, y = valor, fill = ano)) +
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

  salvar_figura_escura(p, nome_arquivo, width = 10.5, height = 5.8)
}

plotar_setor(
  "contratos",
  "Número de operações por subsetor CNAE",
  "fig_top_setor_cnae_operacoes.png",
  dinheiro = FALSE
)

plotar_setor(
  "contratos_unicos",
  "Numero de contratos unicos por subsetor CNAE",
  "fig_top_setor_cnae_contratos_unicos.png",
  dinheiro = FALSE
)

plotar_setor(
  "valor_contratado",
  "Valor contratado por subsetor CNAE",
  "fig_top_setor_cnae_valor_contratado.png"
)

plotar_setor(
  "valor_desembolsado",
  "Valor desembolsado por subsetor CNAE",
  "fig_top_setor_cnae_valor_desembolsado.png"
)
