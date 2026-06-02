args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

df <- base_2024_2025()

metricas <- df |>
  group_by(ano) |>
  summarise(
    contratos = n(),
    valor = sum(valor_contratado, na.rm = TRUE),
    juros = mean(juros_numero, na.rm = TRUE),
    .groups = "drop"
  )

valor_ano <- function(col, ano) metricas[[col]][match(ano, metricas$ano)]

out <- tibble(
  indicador = c(
    "Contratos 2024",
    "Contratos 2025",
    "Delta contratos 2025 vs 2024",
    "Valor contratado 2024",
    "Valor contratado 2025",
    "Delta valor 2025 vs 2024",
    "Juros medio 2024",
    "Juros medio 2025"
  ),
  valor = c(
    fmt_int(valor_ano("contratos", 2024)),
    fmt_int(valor_ano("contratos", 2025)),
    fmt_int(valor_ano("contratos", 2025) - valor_ano("contratos", 2024)),
    fmt_dinheiro(valor_ano("valor", 2024)),
    fmt_dinheiro(valor_ano("valor", 2025)),
    fmt_dinheiro(valor_ano("valor", 2025) - valor_ano("valor", 2024)),
    fmt_pct(valor_ano("juros", 2024)),
    fmt_pct(valor_ano("juros", 2025))
  )
)

salvar_tabela(out, "tab_resumo_executivo.csv")

