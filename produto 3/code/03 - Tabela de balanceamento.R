#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(data.table)
  library(openxlsx)
})

args <- commandArgs(FALSE)
script <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1]), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
out_dir <- file.path(product, "analysis")
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

stars <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .10, "*", ""))))
fmt_diff <- function(x, p) paste0(formatC(x, format = "f", digits = 3, decimal.mark = ","), stars(p))

covars <- fread(file.path(data_dir, "produto3_covariaveis_matching.csv"), sep = ";",
                colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
units <- fread(file.path(data_dir, "produto3_unidades_matching.csv"), sep = ";",
               colClasses = c(cnpj_raiz = "character"), encoding = "UTF-8")
covars[, cnpj_raiz := clean_root(cnpj_raiz)]
units[, cnpj_raiz := clean_root(cnpj_raiz)]
setnames(units, "weights", "match_weight")

vars <- c("idade_firma_pre", "prop_superior", "tempo_medio_trabalhadores",
          "idade_media_trabalhadores", "prop_homens", "prop_pretos")
labels <- c("Idade da empresa", "Proporção com ensino superior",
            "Tempo médio de emprego", "Idade média dos trabalhadores",
            "Proporção de homens", "Proporção de pretos (pardos + negros)")

# A tabela usa somente as coortes da especificação principal.
cohorts <- sort(unique(units$coorte_match))

# Universo de controle: empresas não apoiadas, observadas no ano-base da coorte,
# e localizadas nos mesmos municípios das empresas tratadas.
treated_municipalities <- unique(merge(
  units[treat == 1L, .(g = coorte_match, cnpj_raiz)],
  covars[, .(g, cnpj_raiz, municipio_principal)],
  by = c("g", "cnpj_raiz"), all.x = TRUE
)[!is.na(municipio_principal), .(g, municipio_principal)])
full_control <- covars[g %in% cohorts & treat == 0L][
  treated_municipalities, on = .(g, municipio_principal), nomatch = 0L
]

matched <- merge(
  units[, .(g = coorte_match, cnpj_raiz, treat, match_weight)],
  covars[, c("g", "cnpj_raiz", "municipio_principal", vars), with = FALSE],
  by = c("g", "cnpj_raiz"), all.x = TRUE
)

balance_rows <- rbindlist(lapply(seq_along(vars), function(i) {
  v <- vars[i]
  tr <- matched[treat == 1L & is.finite(get(v))]
  fc <- full_control[is.finite(get(v))]
  mc <- matched[treat == 0L & is.finite(get(v))]

  mean_t <- weighted.mean(tr[[v]], tr$match_weight)
  mean_f <- mean(fc[[v]])
  mean_m <- weighted.mean(mc[[v]], mc$match_weight)
  p_full <- tryCatch(t.test(tr[[v]], fc[[v]])$p.value, error = function(e) NA_real_)
  p_match <- tryCatch(
    summary(lm(c(tr[[v]], mc[[v]]) ~ c(rep(1, nrow(tr)), rep(0, nrow(mc))),
               weights = c(tr$match_weight, mc$match_weight)))$coef[2, 4],
    error = function(e) NA_real_
  )

  data.table(
    Covariada = labels[i],
    Tratamento = mean_t,
    `Controle completo` = mean_f,
    `Controle pareado` = mean_m,
    `Dif. (1-2)` = fmt_diff(mean_t - mean_f, p_full),
    `Dif. (1-3)` = fmt_diff(mean_t - mean_m, p_match)
  )
}))

counts <- data.table(
  Covariada = "Número de empresas (CNPJ-raiz)",
  Tratamento = as.character(uniqueN(matched[treat == 1L, cnpj_raiz])),
  `Controle completo` = as.character(uniqueN(full_control$cnpj_raiz)),
  `Controle pareado` = as.character(uniqueN(matched[treat == 0L, cnpj_raiz])),
  `Dif. (1-2)` = "",
  `Dif. (1-3)` = ""
)

outfile <- file.path(out_dir, "Table 1 - Balanceamento amostra geral.xlsx")
oldfile <- file.path(out_dir, "Table 1 - Balanceamento principal e heterogeneidades.xlsx")
if (file.exists(oldfile)) file.remove(oldfile)

wb <- createWorkbook(creator = "BNDES FUST")
addWorksheet(wb, "Amostra geral", gridLines = FALSE)
s <- "Amostra geral"

title_style <- createStyle(fontName = "Arial", fontSize = 13, textDecoration = "bold",
                           fontColour = cor_azul, halign = "center", valign = "center")
number_style <- createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold",
                            halign = "center", border = "bottom", borderStyle = "thin")
header_style <- createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold",
                            fontColour = "#FFFFFF", fgFill = cor_azul,
                            halign = "center", valign = "center", wrapText = TRUE,
                            border = "bottom", borderStyle = "medium")
label_style <- createStyle(fontName = "Arial", fontSize = 11, halign = "left")
value_style <- createStyle(fontName = "Arial", fontSize = 11, halign = "center",
                           numFmt = "0.000")
diff_style <- createStyle(fontName = "Arial", fontSize = 11, halign = "center")
count_style <- createStyle(fontName = "Arial", fontSize = 11, textDecoration = "bold",
                           border = c("top", "bottom"), borderStyle = "thin")
note_style <- createStyle(fontName = "Arial", fontSize = 9, textDecoration = "italic",
                          wrapText = TRUE, valign = "top")

writeData(wb, s, "Teste de balanceamento antes e depois do pareamento", startRow = 1, startCol = 1)
mergeCells(wb, s, cols = 1:6, rows = 1)
addStyle(wb, s, title_style, rows = 1, cols = 1:6, gridExpand = TRUE)
setRowHeights(wb, s, 1, 24)

writeData(wb, s, matrix(c("", "(1)", "(2)", "(3)", "(4)", "(5)"), nrow = 1),
          startRow = 3, startCol = 1, colNames = FALSE)
addStyle(wb, s, number_style, rows = 3, cols = 1:6, gridExpand = TRUE)
writeData(wb, s,
          matrix(c("Covariada", "Tratamento", "Controle completo", "Controle pareado",
                   "Dif. (1-2)", "Dif. (1-3)"), nrow = 1),
          startRow = 4, startCol = 1, colNames = FALSE)
addStyle(wb, s, header_style, rows = 4, cols = 1:6, gridExpand = TRUE)
setRowHeights(wb, s, 4, 34)

first_data <- 5
last_data <- first_data + nrow(balance_rows) - 1
writeData(wb, s, balance_rows, startRow = first_data, startCol = 1, colNames = FALSE)
addStyle(wb, s, label_style, rows = first_data:last_data, cols = 1, gridExpand = TRUE)
addStyle(wb, s, value_style, rows = first_data:last_data, cols = 2:4, gridExpand = TRUE)
addStyle(wb, s, diff_style, rows = first_data:last_data, cols = 5:6, gridExpand = TRUE)

count_row <- last_data + 2
writeData(wb, s, counts, startRow = count_row, startCol = 1, colNames = FALSE)
addStyle(wb, s, count_style, rows = count_row, cols = 1:6, gridExpand = TRUE)
addStyle(wb, s, createStyle(halign = "center"), rows = count_row, cols = 2:6, gridExpand = TRUE, stack = TRUE)

note_row <- count_row + 2
note <- paste0(
  "Nota: a tabela apresenta as médias das covariadas para as empresas apoiadas pelo FUST (coluna 1), ",
  "para todas as empresas não apoiadas elegíveis nas mesmas coortes e nos mesmos municípios das tratadas ",
  "(coluna 2) e para os controles selecionados pelo algoritmo de pareamento (coluna 3). As colunas 4 e 5 ",
  "mostram as diferenças de médias (1)-(2) e (1)-(3). A unidade é o CNPJ-raiz; empresas presentes em mais ",
  "de uma coorte são contadas uma única vez na linha de tamanho da amostra. Pretos = pardos + negros. ",
  "*** p<0,01; ** p<0,05; * p<0,10."
)
writeData(wb, s, note, startRow = note_row, startCol = 1)
mergeCells(wb, s, cols = 1:6, rows = note_row:(note_row + 3))
addStyle(wb, s, note_style, rows = note_row:(note_row + 3), cols = 1:6, gridExpand = TRUE)

setColWidths(wb, s, cols = 1:6, widths = c(41, 17, 20, 20, 16, 16))
setRowHeights(wb, s, rows = first_data:last_data, heights = 22)
freezePane(wb, s, firstActiveRow = 5)
pageSetup(wb, s, orientation = "landscape", fitToWidth = 1, fitToHeight = 1)
setHeaderFooter(wb, s, footer = c("", "Página &P de &N", ""))

saveWorkbook(wb, outfile, overwrite = TRUE)
cat(sprintf("Arquivo salvo: %s\n", outfile))
cat(sprintf("Amostra: %d tratadas; %d controles completos; %d controles pareados; %d municípios tratados.\n",
            uniqueN(matched[treat == 1L, cnpj_raiz]), uniqueN(full_control$cnpj_raiz),
            uniqueN(matched[treat == 0L, cnpj_raiz]), uniqueN(treated_municipalities$municipio_principal)))
