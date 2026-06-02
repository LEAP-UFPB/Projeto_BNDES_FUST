args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- ler_base() |>
  resumir(ano, origem_detalhada) |>
  arrange(ano, origem_detalhada) |>
  formatar_tabela()

salvar_tabela(out, "tab_ano_origem.csv")

