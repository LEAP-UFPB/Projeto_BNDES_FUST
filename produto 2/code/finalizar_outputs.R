args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
analysis_dir <- dirname(dirname(script_path))
output_dir <- file.path(analysis_dir, "analysis")

if (!requireNamespace("png", quietly = TRUE)) {
  install.packages("png")
}

arquivos <- list.files(
  output_dir,
  pattern = "[.]png$",
  recursive = TRUE,
  full.names = TRUE
)

normalizados <- 0L

for (arquivo in arquivos) {
  imagem <- png::readPNG(arquivo)
  dimensoes <- dim(imagem)

  if (length(dimensoes) != 3L || !dimensoes[3] %in% c(3L, 4L)) {
    next
  }

  possui_transparencia <- dimensoes[3] == 4L && any(imagem[, , 4] < 1)
  if (possui_transparencia) {
    next
  }

  alfa <- ifelse(
    imagem[, , 1] >= 250 / 255 &
      imagem[, , 2] >= 250 / 255 &
      imagem[, , 3] >= 250 / 255,
    0,
    1
  )

  saida <- array(0, dim = c(dimensoes[1], dimensoes[2], 4L))
  saida[, , 1:3] <- imagem[, , 1:3]
  saida[, , 4] <- alfa
  png::writePNG(saida, arquivo)
  normalizados <- normalizados + 1L
}

message("PNGs com transparência normalizada: ", normalizados)
