args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))

pacotes_mapa <- c("geobr", "sf")
faltando_mapa <- setdiff(pacotes_mapa, rownames(installed.packages()))
if (length(faltando_mapa) > 0) {
  install.packages(faltando_mapa)
}

suppressPackageStartupMessages({
  library(geobr)
  library(sf)
})

df <- base_2024_2025() |>
  filter(!is.na(uf)) |>
  group_by(uf) |>
  summarise(
    contratos = n(),
    valor_contratado = sum(valor_contratado, na.rm = TRUE),
    .groups = "drop"
  )

ufs <- read_state(year = 2020, showProgress = FALSE) |>
  left_join(df, by = c("abbrev_state" = "uf")) |>
  mutate(valor_contratado = replace_na(valor_contratado, 0))

ufs <- ufs |>
  mutate(rotulo_uf = abbrev_state)

p <- ggplot(ufs) +
  geom_sf(aes(fill = valor_contratado), color = "white", linewidth = 0.25) +
  geom_sf_text(aes(label = rotulo_uf), size = 2.7, color = "black", lineheight = 0.85) +
  scale_fill_gradient(
    low = "#E8EEF9",
    high = cor_azul,
    labels = label_number(scale = 1e-6, suffix = " mi", big.mark = ".", decimal.mark = ","),
    guide = guide_colorbar(
      direction = "horizontal",
      barheight = unit(2, units = "mm"),
      barwidth = unit(50, units = "mm"),
      draw.ulim = FALSE,
      title.position = "top",
      title.hjust = 0.5,
      label.hjust = 0.5
    )
  ) +
  labs(title = "Valor contratado por UF", subtitle = "Contratos de 2024 e 2025", fill = "Valor") +
  tema_fust() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

salvar_figura(p, "map_valor_contratado_por_uf.png", width = 8, height = 8)

