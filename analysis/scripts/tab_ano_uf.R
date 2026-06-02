args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  filter(!is.na(uf)) |>
  resumir(ano, uf) |>
  arrange(ano, desc(valor_contratado)) |>
  formatar_tabela()

salvar_tabela(out, "tab_ano_uf.csv")

