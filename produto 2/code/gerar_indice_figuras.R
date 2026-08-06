args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
produto_dir <- dirname(dirname(script_path))
analysis_dir <- file.path(produto_dir, "analysis")
saida <- file.path(produto_dir, "FIGURAS_E_TITULOS.md")

figuras <- list.files(
  analysis_dir,
  pattern = "[.]png$",
  recursive = TRUE,
  full.names = FALSE
)

codigo_da_figura <- function(caminho) {
  nome <- basename(caminho)
  if (grepl(
    "Quantidade de indicadores|Cobertura dos indicadores|Previsto versus realizado|^TABELA (23|30)[.]",
    nome
  )) {
    return("code/quadro_resultados_analises.R")
  }
  if (grepl("tipo de operação", nome)) {
    if (grepl("Número de operações", nome)) {
      return("code/fig_composicao_por_origem.R")
    }
    return("code/fig_modalidade_contratado_vs_desembolsado.R")
  }
  if (grepl("subsetor CNAE", nome)) {
    return("code/fig_top_setor_cnae_valor_contratado.R")
  }
  if (grepl("porte dos clientes", nome)) {
    return("code/fig_composicao_por_porte.R")
  }
  if (grepl("Distribuição regional e por UF", nome)) {
    if (grepl("mapa", nome, ignore.case = TRUE)) {
      return("code/map_valor_contratado_por_uf.R")
    }
    return("code/fig_top_uf_valor_contratado.R")
  }
  if (grepl("taxa de juros|prazo total|carência|frequência", nome, ignore.case = TRUE)) {
    return("code/fig_juros_e_prazo_total.R")
  }
  "code/finalizar_outputs.R"
}

numero_figura <- function(nome) {
  achado <- regmatches(nome, regexpr("Figura [0-9]+", nome))
  if (length(achado) == 0 || achado == "") "—" else achado
}

linhas <- c(
  "# Figuras, títulos e códigos — Produto 2",
  "",
  "Índice gerado automaticamente a partir de `analysis/`.",
  "",
  "| Código | Figura | Título | Arquivo |",
  "|---|---:|---|---|"
)

for (figura in sort(figuras)) {
  titulo <- sub("[.]png$", "", basename(figura))
  linhas <- c(
    linhas,
    paste0(
      "| `", codigo_da_figura(figura), "` | ",
      numero_figura(titulo), " | ",
      gsub("[|]", "\\\\|", titulo), " | `analysis/",
      gsub("\\\\", "/", figura), "` |"
    )
  )
}

writeLines(enc2utf8(linhas), saida, useBytes = TRUE)
message("Índice criado em: ", saida)
