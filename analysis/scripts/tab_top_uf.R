args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  filter(!is.na(uf)) |>
  resumir(uf) |>
  slice_max(valor_contratado, n = 15) |>
  arrange(desc(valor_contratado)) |>
  formatar_tabela()

salvar_tabela(out, "tab_top_uf.csv")

