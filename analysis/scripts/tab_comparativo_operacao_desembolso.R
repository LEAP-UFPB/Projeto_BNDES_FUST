args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  group_by(ano) |>
  summarise(
    contratos = n(),
    valor_operacao = sum(valor_operacao, na.rm = TRUE),
    valor_desembolsado = sum(valor_desembolsado, na.rm = TRUE),
    diferenca_operacao_desembolso = valor_operacao - valor_desembolsado,
    percentual_desembolsado = if_else(valor_operacao > 0, valor_desembolsado / valor_operacao * 100, NA_real_),
    .groups = "drop"
  ) |>
  mutate(
    contratos = fmt_int(contratos),
    valor_operacao = fmt_dinheiro(valor_operacao),
    valor_desembolsado = fmt_dinheiro(valor_desembolsado),
    diferenca_operacao_desembolso = fmt_dinheiro(diferenca_operacao_desembolso),
    percentual_desembolsado = fmt_pct(percentual_desembolsado)
  )

salvar_tabela(out, "tab_comparativo_operacao_desembolso.csv")
