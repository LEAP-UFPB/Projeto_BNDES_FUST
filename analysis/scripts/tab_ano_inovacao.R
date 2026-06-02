args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  resumir(ano, inovacao_limpa) |>
  arrange(ano, inovacao_limpa) |>
  formatar_tabela()

salvar_tabela(out, "tab_ano_inovacao.csv")

