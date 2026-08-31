# Configuração compartilhada do Produto 3.

cor_verde <- "#127D3E"
cor_azul <- "#2A4089"
cor_fundo <- "#FFFFFF"
cor_grade <- "#D9D9D9"
cor_texto <- "#000000"
cor_verde_claro <- "#67B884"
cor_azul_claro <- "#8FA3D9"
cor_verde_escuro <- "#0B4F27"
cor_azul_escuro <- "#182757"

paleta_bndes <- c(
  cor_azul, cor_verde, cor_azul_claro,
  cor_verde_claro, cor_azul_escuro, cor_verde_escuro
)

outcomes_produto3 <- data.table::data.table(
  outcome = c("log_emprego", "log_massa_salarial", "log_salario_medio"),
  label = c("Log emprego", "Log massa salarial", "Log salário médio"),
  safe = c("Log-emprego", "Log-massa-salarial", "Log-salario-medio")
)

tema_bndes <- function(legend_position = "bottom", base_size = 12) {
  ggplot2::theme_minimal(base_family = "Arial", base_size = base_size) +
    ggplot2::theme(
      legend.position = legend_position,
      legend.title = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = cor_grade, linewidth = .5),
      plot.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = cor_texto),
      plot.background = ggplot2::element_rect(fill = cor_fundo, color = NA),
      panel.background = ggplot2::element_rect(fill = cor_fundo, color = NA)
    )
}

clean_root <- function(x) {
  sprintf("%08d", suppressWarnings(as.integer(gsub("\\D", "", as.character(x)))))
}
