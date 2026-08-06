args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  install.packages("openxlsx")
}

anos <- 2023:2026
base_file <- file.path(data_dir, "Base de Operações_FUST_compilada.xlsx")
rais_file <- file.path(data_dir, "RAIS_matching_tratadas_controles_2018_2024.xlsx")
emprego_file <- file.path(data_dir, "RAIS_matching_emprego_mensal_2022_2024.xlsx")

if (!file.exists(base_file) || !file.exists(rais_file) || !file.exists(emprego_file)) {
  stop("Bases necessárias aos Quadros 2 a 5 não encontradas em data/.", call. = FALSE)
}

base <- openxlsx::read.xlsx(base_file, detectDates = TRUE)
rais <- openxlsx::read.xlsx(rais_file)

texto <- function(x) trimws(enc2utf8(as.character(x)))
cnpj <- function(x) {
  y <- gsub("[^0-9]", "", texto(x))
  y[nchar(y) == 0L] <- NA_character_
  sprintf("%014s", y)
}
chave_cliente <- function(x) vapply(texto(x), function(valor) {
  partes <- trimws(strsplit(valor, "|", fixed = TRUE)[[1]])
  mascarada <- partes[grepl("*", partes, fixed = TRUE)]
  if (length(mascarada)) return(toupper(gsub("\\s+", "", mascarada[1])))
  digitos <- gsub("[^0-9]", "", partes[1])
  if (nchar(digitos) == 14L) {
    return(sprintf("**.*%s.%s/%s-**", substr(digitos, 4, 5), substr(digitos, 6, 8), substr(digitos, 9, 12)))
  }
  toupper(gsub("\\s+", "", partes[1]))
}, character(1))
numero <- function(x) suppressWarnings(as.numeric(gsub(",", ".", texto(x), fixed = TRUE)))

data_contrato <- base$data_da_contratacao
if (is.numeric(data_contrato)) {
  data_contrato <- openxlsx::convertToDate(data_contrato)
} else {
  data_contrato <- as.Date(data_contrato, format = "%d/%m/%Y")
}

datas_qr <- lapply(list.files(data_dir, pattern = "^Base de QR FUST .*[.]xlsx$", full.names = TRUE), function(f) {
  x <- openxlsx::read.xlsx(f, sheet = 1, detectDates = TRUE)
  if (!all(c("operação.número", "data.aprovacao") %in% names(x))) return(NULL)
  data.frame(
    operacao = suppressWarnings(as.numeric(x[["operação.número"]])),
    data = as.Date(x[["data.aprovacao"]]),
    stringsAsFactors = FALSE
  )
})
datas_qr <- do.call(rbind, Filter(Negate(is.null), datas_qr))
if (!is.null(datas_qr) && nrow(datas_qr)) {
  datas_qr <- aggregate(data ~ operacao, datas_qr, min, na.rm = TRUE)
  chave_operacao <- suppressWarnings(as.numeric(base$numero_de_operacao))
  data_qr <- datas_qr$data[match(chave_operacao, datas_qr$operacao)]
  data_contrato[!is.na(data_qr)] <- data_qr[!is.na(data_qr)]
}

operacoes <- data.frame(
  tipo = texto(base$tipo_operacao),
  ano = as.integer(format(data_contrato, "%Y")),
  operacao = texto(base$numero_de_operacao),
  cliente = chave_cliente(base$cnpj),
  stringsAsFactors = FALSE
)
operacoes$operacao[is.na(operacoes$operacao) | operacoes$operacao == ""] <-
  paste0("linha_", which(is.na(operacoes$operacao) | operacoes$operacao == ""))

primeiro_ano <- tapply(operacoes$ano, operacoes$cliente, min, na.rm = TRUE)

contar <- function(tipo, ano, campo) {
  x <- operacoes[operacoes$tipo == tipo & operacoes$ano == ano, campo]
  length(unique(x[!is.na(x) & x != ""]))
}

contar_novos <- function(tipo, ano) {
  clientes <- unique(operacoes$cliente[operacoes$tipo == tipo & operacoes$ano == ano])
  clientes <- clientes[!is.na(clientes)]
  sum(primeiro_ano[clientes] == ano, na.rm = TRUE)
}

rais_limpa <- data.frame(
  papel = tolower(texto(rais$papel)),
  dimensao = tolower(texto(rais$dimensao)),
  tipo = texto(rais$tipo_primeira_operacao),
  ano = suppressWarnings(as.integer(rais$ano)),
  cnpj = cnpj(rais$cnpj_raiz),
  valor = numero(rais$variacao_pct_empregados),
  stringsAsFactors = FALSE
)

emprego <- openxlsx::read.xlsx(emprego_file, sheet = "emprego_mensal")
emprego_limpo <- data.frame(
  cnpj = cnpj(emprego$cnpj_raiz),
  ano = suppressWarnings(as.integer(emprego$ano)),
  estoque = numero(emprego$estoque_medio_anual_empregados),
  stringsAsFactors = FALSE
)
emprego_limpo <- emprego_limpo[order(emprego_limpo$cnpj, emprego_limpo$ano), ]
emprego_limpo$valor <- ave(
  emprego_limpo$estoque,
  emprego_limpo$cnpj,
  FUN = function(x) c(NA_real_, x[-1L] / x[-length(x)] - 1)
)

mediana_rais <- function(papel, tipo, ano) {
  ids <- rais_limpa[
    rais_limpa$papel == papel &
      rais_limpa$dimensao == "geral" &
      (if (papel == "tratada") rais_limpa$tipo == tipo else TRUE),
    "cnpj"
  ]
  x <- emprego_limpo[emprego_limpo$cnpj %in% unique(ids) & emprego_limpo$ano == ano, ]
  if (!any(is.finite(x$valor))) return(NA_real_)
  stats::median(x$valor, na.rm = TRUE)
}

percentual <- function(x) {
  ifelse(is.na(x), "*", paste0(format(round(100 * x, 1), nsmall = 1, decimal.mark = ","), "%"))
}

linhas <- data.frame(
  Resultado = c(rep("Eficacia", 5), rep("Efetividade", 10)),
  Indicador = c(
    "I1 - numero de operacoes aprovadas",
    "I2 - numero de clientes apoiados",
    "I3 - novos clientes",
    "I4 - equipamentos adquiridos ou modernizados por tipo",
    "I5 - area construida ou modernizada em projetos de investimento",
    "I6.med.ant - mediana da variacao percentual do estoque medio anual de empregados das empresas apoiadas - ano de referencia vs anterior",
    "I6.med.pos - mediana da variacao percentual do estoque medio anual de empregados das empresas apoiadas - ano posterior vs referencia",
    "I6.if5.ant - media da variacao percentual do estoque medio anual de empregados das empresas apoiadas - ano de referencia vs anterior",
    "I6.if5.pos - media da variacao percentual do estoque medio anual de empregados das empresas apoiadas - ano posterior vs referencia",
    "I7.med.ant - mediana da variacao percentual do estoque medio anual de empregados das empresas nao apoiadas pareadas - ano de referencia vs anterior",
    "I7.med.pos - mediana da variacao percentual do estoque medio anual de empregados das empresas nao apoiadas pareadas - ano posterior vs referencia",
    "I7.if5.ant - media da variacao percentual do estoque medio anual de empregados das empresas nao apoiadas pareadas - ano de referencia vs anterior",
    "I7.if5.pos - media da variacao percentual do estoque medio anual de empregados das empresas nao apoiadas pareadas - ano posterior vs referencia",
    "I8 - variacao percentual da massa salarial das empresas apoiadas",
    "I9 - variacao percentual da massa salarial das empresas nao apoiadas pareadas"
  ),
  stringsAsFactors = FALSE
)

gerar_quadro <- function(tipo, arquivo, aba) {
  valores <- matrix("*", nrow = nrow(linhas), ncol = length(anos))
  for (j in seq_along(anos)) {
    ano <- anos[j]
    valores[1, j] <- contar(tipo, ano, "operacao")
    valores[2, j] <- contar(tipo, ano, "cliente")
    valores[3, j] <- contar_novos(tipo, ano)
  }

  # A RAIS disponível termina em 2024. As linhas "pos" usam o ano seguinte.
  for (j in seq_along(anos)) {
    ano <- anos[j]
    valores[6, j] <- percentual(mediana_rais("tratada", tipo, ano))
    valores[7, j] <- percentual(mediana_rais("tratada", tipo, ano + 1L))
    valores[10, j] <- percentual(mediana_rais("controle", tipo, ano))
    valores[11, j] <- percentual(mediana_rais("controle", tipo, ano + 1L))
  }

  quadro <- cbind(linhas, as.data.frame(valores, check.names = FALSE))
  names(quadro)[3:6] <- as.character(anos)

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, aba, gridLines = FALSE)
  openxlsx::writeData(wb, aba, quadro)
  openxlsx::addStyle(
    wb, aba,
    openxlsx::createStyle(
      textDecoration = "bold", fontColour = "#FFFFFF",
      fgFill = cor_azul, halign = "center", wrapText = TRUE
    ),
    rows = 1, cols = seq_len(ncol(quadro)), gridExpand = TRUE
  )
  openxlsx::setColWidths(wb, aba, cols = 1, widths = 16)
  openxlsx::setColWidths(wb, aba, cols = 2, widths = 105)
  openxlsx::setColWidths(wb, aba, cols = 3:6, widths = 13)
  openxlsx::freezePane(wb, aba, firstActiveRow = 2, firstActiveCol = 3)
  openxlsx::saveWorkbook(wb, file.path(output_dir, arquivo), overwrite = TRUE)
}

gerar_quadro(
  "Direta",
  "Quadros 2 e 3 - Teoria da Mudança - apoio direto.xlsx",
  "Direta"
)
gerar_quadro(
  "Indireta",
  "Quadros 4 e 5 - Teoria da Mudança - apoio indireto.xlsx",
  "Indireta"
)

message("Quadros 2 a 5 gerados em: ", output_dir)
