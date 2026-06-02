args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  resumir(ano, juros_faixa) |>
  arrange(ano, juros_faixa) |>
  formatar_tabela()

salvar_tabela(out, "tab_faixas_juros.csv")

