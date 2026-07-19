args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

out <- base_2024_2025() |>
  resumir(ano, juros_faixa) |>
  arrange(ano, juros_faixa) |>
  mutate(
    valor_contratado = number(valor_contratado / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 0.1),
    valor_desembolsado = number(valor_desembolsado / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 0.1),
    ticket_medio = number(ticket_medio / 1e6, big.mark = ".", decimal.mark = ",", accuracy = 0.1),
    juros_medio = fmt_pct(juros_medio),
    juros_medio_ponderado = fmt_pct(juros_medio_ponderado),
    contratos = fmt_int(contratos)
  ) |>
  rename(
    `valor_contratado_r_milhoes` = valor_contratado,
    `valor_desembolsado_r_milhoes` = valor_desembolsado,
    `ticket_medio_r_milhoes` = ticket_medio
  )

salvar_tabela(out, "tab_faixas_juros.csv")

