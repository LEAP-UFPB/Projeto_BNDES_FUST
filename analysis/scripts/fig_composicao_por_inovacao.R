args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

cores_inovacao <- c(
  "Sim" = cor_verde,
  "Nao" = cor_azul,
  "N\u00e3o" = cor_azul,
  "Nao informado" = cor_azul_claro,
  "N\u00e3o informado" = cor_azul_claro
)

plotar_composicao_inovacao <- function(nome_arquivo, contratos_unicos = FALSE) {
  base <- base_2024_2025() |>
    mutate(inovacao_limpa = if_else(is.na(inovacao_limpa) | inovacao_limpa == "", "Nao informado", inovacao_limpa))

  df <- if (contratos_unicos) {
    contar_contratos_unicos(base, ano, inovacao_limpa, name = "contratos")
  } else {
    base |>
      count(ano, inovacao_limpa, name = "contratos")
  }

  df <- df |>
    group_by(ano) |>
    mutate(
      frac = contratos / sum(contratos),
      label = if_else(contratos > 0, paste0(fmt_int(contratos), "\n", percent(frac, accuracy = 0.1, decimal.mark = ",")), "")
    ) |>
    ungroup() |>
    mutate(ano = factor(ano, levels = c(2024L, 2025L)))

  paleta_inovacao <- cores_inovacao[names(cores_inovacao) %in% unique(df$inovacao_limpa)]
  faltantes <- setdiff(unique(df$inovacao_limpa), names(paleta_inovacao))
  if (length(faltantes) > 0) {
    paleta_inovacao <- c(paleta_inovacao, setNames(paleta_fust[seq_along(faltantes)], faltantes))
  }

  p <- ggplot(df, aes(x = 2, y = frac, fill = inovacao_limpa)) +
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
    scale_fill_manual(values = paleta_inovacao, drop = FALSE) +
    labs(title = "Composicao por inovacao", fill = NULL) +
    tema_donut_escuro()

  salvar_figura_escura(p, nome_arquivo, width = 7, height = 4.5)
}

plotar_composicao_inovacao("fig_composicao_por_inovacao.png")
plotar_composicao_inovacao("fig_composicao_por_inovacao_contratos_unicos.png", contratos_unicos = TRUE)
