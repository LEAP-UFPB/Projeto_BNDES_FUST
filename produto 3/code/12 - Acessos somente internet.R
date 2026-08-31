#!/usr/bin/env Rscript

# Analise adicional de DiD restrita ao produto INTERNET da base de acessos de
# banda larga fixa da Anatel. A rotina principal e reutilizada com saidas e
# caches separados, preservando integralmente a analise com todos os produtos.

Sys.setenv(ANATEL_TIPO_PRODUTO = "INTERNET")
args <- commandArgs(FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) stop("Execute este arquivo com Rscript.")
script_dir <- dirname(normalizePath(
  sub("^--file=", "", script_arg), winslash = "/"
))
source(
  file.path(script_dir, "08 - Acessos banda larga fixa.R"),
  encoding = "UTF-8"
)
