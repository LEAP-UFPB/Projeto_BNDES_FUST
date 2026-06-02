args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- comparar_2024_2025(ler_base(), "origem_detalhada") |>
  mutate(
    across(c(valor_2024, valor_2025, delta_valor), fmt_dinheiro),
    across(c(juros_2024, juros_2025), fmt_pct),
    across(c(contratos_2024, contratos_2025, delta_contratos), fmt_int)
  )

salvar_tabela(out, "tab_comparativo_origem.csv")

