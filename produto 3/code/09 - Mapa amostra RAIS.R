#!/usr/bin/env Rscript

# Localizacao das empresas da amostra oficial das estimacoes da RAIS.
# A unidade e a raiz do CNPJ e a localizacao corresponde ao municipio principal
# observado na RAIS antes do primeiro tratamento.

suppressPackageStartupMessages({
  library(data.table)
  library(sf)
  library(ggplot2)
  library(ggrepel)
  library(scales)
})

args <- commandArgs(FALSE)
script_arg <- args[grepl("^--file=", args)][1]
if (is.na(script_arg)) stop("Execute este arquivo com Rscript.")
script <- normalizePath(sub("^--file=", "", script_arg), winslash = "/")
product <- normalizePath(file.path(dirname(script), ".."), winslash = "/")
repo <- normalizePath(file.path(product, ".."), winslash = "/")
data_dir <- file.path(repo, "data")
output_dir <- file.path(product, "analysis")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
source(file.path(dirname(script), "_common.R"), encoding = "UTF-8")

units_file <- file.path(data_dir, "produto3_unidades_matching.csv")
municipal_shape <- file.path(data_dir, "mapas", "BR_Municipios_2021.shp")
state_shape <- file.path(data_dir, "mapas", "BR_UF_2021.shp")
output_files <- c(
  "Tratadas pelo FUST" = file.path(
    output_dir, "Mapa - Localizacao empresas tratadas RAIS.png"
  ),
  "Controles pareados" = file.path(
    output_dir, "Mapa - Localizacao controles pareados RAIS.png"
  )
)

required_files <- c(units_file, municipal_shape, state_shape)
if (any(!file.exists(required_files))) {
  stop("Arquivos ausentes: ", paste(required_files[!file.exists(required_files)], collapse = ", "))
}

sample <- fread(
  units_file, sep = ";",
  colClasses = c(cnpj_raiz = "character", municipio_principal = "character")
)
needed <- c("cnpj_raiz", "municipio_principal", "treat")
if (!all(needed %in% names(sample))) {
  stop("A amostra RAIS nao contem: ", paste(setdiff(needed, names(sample)), collapse = ", "))
}
sample[, cnpj_raiz := sprintf("%08d", suppressWarnings(as.integer(gsub("\\D", "", cnpj_raiz))))]
sample[, municipio_id := sprintf("%06d", suppressWarnings(as.integer(municipio_principal)))]
sample <- unique(sample[, ..needed][, municipio_id := sprintf(
  "%06d", suppressWarnings(as.integer(municipio_principal))
)], by = "cnpj_raiz")
sample[, grupo := factor(
  fifelse(treat == 1L, "Tratadas pelo FUST", "Controles pareados"),
  levels = c("Tratadas pelo FUST", "Controles pareados")
)]

sample_sizes <- sample[, .(empresas = uniqueN(cnpj_raiz)), by = grupo]
if (nrow(sample_sizes) != 2L || any(sample_sizes$empresas != 224L)) {
  stop("A amostra deixou de ser 1:1 com 224 empresas em cada grupo.")
}

locations <- sample[, .(empresas = uniqueN(cnpj_raiz)), by = .(grupo, municipio_id)]

# Carrega a malha oficial local. Os codigos da amostra usam os seis primeiros
# digitos do codigo IBGE; a malha inclui tambem o digito verificador.
states <- st_read(state_shape, quiet = TRUE, options = "ENCODING=UTF-8")
municipalities <- st_read(municipal_shape, quiet = TRUE, options = "ENCODING=UTF-8")
municipalities$municipio_id <- substr(as.character(municipalities$CD_MUN), 1L, 6L)
municipalities <- municipalities[municipalities$municipio_id %in% locations$municipio_id, ]

missing_municipalities <- setdiff(locations$municipio_id, municipalities$municipio_id)
if (length(missing_municipalities)) {
  stop("Municipios da amostra ausentes na malha: ", paste(missing_municipalities, collapse = ", "))
}

# A projecao policonica brasileira evita distorcoes na obtencao dos pontos e
# deixa o mapa nacional com proporcoes adequadas.
crs_map <- 5880
states <- st_transform(states, crs_map)
municipalities <- st_transform(municipalities, crs_map)
municipal_points <- suppressWarnings(st_point_on_surface(
  municipalities[, c("municipio_id", "NM_MUN", "SIGLA")]
))

points <- merge(
  municipal_points, locations,
  by = "municipio_id", all.y = TRUE, sort = FALSE
)
if (any(st_is_empty(points))) stop("Ha municipios sem geometria no mapa.")

state_labels <- suppressWarnings(st_point_on_surface(states[, c("SIGLA")]))

# A malha estadual inclui centenas de ilhotas e arquipelagos que, nesta escala,
# aparecem como pontos pretos soltos no oceano. Mantem componentes territoriais
# com pelo menos 100 km2; os rotulos estaduais continuam baseados na geometria
# original e os pontos empresariais nao sao filtrados.
state_parts <- suppressWarnings(st_cast(states, "POLYGON"))
state_parts$component_area_km2 <- as.numeric(st_area(state_parts)) / 1e6
states <- state_parts[state_parts$component_area_km2 >= 100, ]

# Rotula os cinco municipios com mais empresas em cada grupo. Em caso de
# empate, a ordenacao alfabetica torna a escolha reproduzivel.
label_keys <- locations[order(grupo, -empresas, municipio_id), head(.SD, 5L), by = grupo]
labels <- points[paste(points$grupo, points$municipio_id) %in%
                   paste(label_keys$grupo, label_keys$municipio_id), ]
label_xy <- st_coordinates(labels)
label_data <- as.data.table(cbind(
  st_drop_geometry(labels), x = label_xy[, 1], y = label_xy[, 2]
))
label_data[, rotulo := paste0(NM_MUN, " (", empresas, ")")]

group_colors <- c(
  "Tratadas pelo FUST" = cor_verde,
  "Controles pareados" = cor_azul
)
size_breaks <- c(1, 5, 10, 20, 30)

make_map <- function(group_name) {
  group_points <- points[as.character(points$grupo) == group_name, ]
  group_labels <- label_data[as.character(grupo) == group_name]
  group_color <- unname(group_colors[[group_name]])

  ggplot() +
    geom_sf(
      data = states, fill = "white", color = "black",
      linewidth = .62
    ) +
    geom_sf_text(
      data = state_labels, aes(label = SIGLA),
      color = "#777777", size = 2.35, fontface = "bold"
    ) +
    geom_sf(
      data = group_points,
      aes(size = empresas), color = group_color,
      alpha = .84, stroke = .2
    ) +
    geom_label_repel(
      data = group_labels,
      aes(x = x, y = y, label = rotulo),
      color = group_color, size = 2.8, fontface = "bold",
      fill = alpha("white", .95),
      label.size = .22, label.padding = unit(1.5, "mm"),
      box.padding = .4, point.padding = .18, min.segment.length = 0,
      segment.color = "#555555", segment.size = .35,
      max.overlaps = Inf, seed = 3854035, show.legend = FALSE
    ) +
    scale_size_area(
      max_size = 10, breaks = size_breaks,
      limits = c(1, max(locations$empresas)),
      name = "Empresas no município"
    ) +
    # Recorta os arquipelagos oceanicos distantes (Fernando de Noronha,
    # Trindade e Martim Vaz), que aparecem como pontos pretos soltos na escala
    # nacional. Nenhuma empresa da amostra fica fora deste enquadramento.
    coord_sf(
      crs = st_crs(crs_map), xlim = c(2.75e6, 7.20e6),
      datum = NA, clip = "on"
    ) +
    theme_minimal(base_family = "Arial", base_size = 11) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = .62),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(10, 12, 8, 12)
    )
}

for (group_name in names(output_files)) {
  p <- make_map(group_name)
  ggsave(output_files[[group_name]], p, width = 7, height = 7.5, dpi = 320, bg = "white")
  cat("Mapa salvo em: ", output_files[[group_name]], "\n", sep = "")
}
print(sample_sizes)
