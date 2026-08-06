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

linhas <- c(
  "# Figuras, títulos e códigos — Produto 3",
  "",
  "| Código | Análise | Título | Arquivo |",
  "|---|---|---|---|"
)

for (figura in sort(figuras)) {
  frequencia <- grepl("^frequencias_pos_covid", figura)
  codigo <- if (frequencia) {
    "code/frequencias_pos_covid/run_did_frequencias.R"
  } else {
    "code/psm_did/run_psm_did_fust.R"
  }
  analise <- if (frequencia) "DID em frequência mensal" else "PSM-DID anual"
  titulo <- sub("[.]png$", "", basename(figura))
  linhas <- c(
    linhas,
    paste0(
      "| `", codigo, "` | ", analise, " | ",
      gsub("[|]", "\\\\|", titulo), " | `analysis/",
      gsub("\\\\", "/", figura), "` |"
    )
  )
}

writeLines(enc2utf8(linhas), saida, useBytes = TRUE)
message("Índice criado em: ", saida)
