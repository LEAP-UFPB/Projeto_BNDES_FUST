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
  group_by(ano, uf) |>
  summarise(
    contratos = n(),
    valor_contratado = sum(valor_contratado, na.rm = TRUE),
    .groups = "drop"
  )

limite_mapa <- max(df$valor_contratado, na.rm = TRUE)

ler_estados <- function() {
  estados_geobr <- tryCatch(
    read_state(year = 2020, simplified = TRUE, showProgress = FALSE, cache = FALSE),
    error = function(e) NULL
  )

  if (!is.null(estados_geobr)) {
    return(estados_geobr |>
      mutate(rotulo_uf = abbrev_state))
  }

  shp_local <- file.path(data_dir, "mapas", "BR_UF_2021.shp")
  if (!file.exists(shp_local)) {
    stop("Nao foi possivel carregar a malha de UFs pelo geobr nem pelo fallback local.", call. = FALSE)
  }

  st_read(shp_local, quiet = TRUE) |>
    rename(abbrev_state = SIGLA) |>
    mutate(rotulo_uf = abbrev_state)
}

estados <- ler_estados()

mapas <- lapply(c(2024L, 2025L), function(ano_mapa) {
  dados_ano <- df |>
    filter(ano == ano_mapa)

  estados |>
    left_join(dados_ano, by = c("abbrev_state" = "uf")) |>
    mutate(
      ano = ano_mapa,
      valor_contratado = replace_na(valor_contratado, 0)
    )
})

ufs_anuais <- do.call(rbind, mapas) |>
  mutate(ano = factor(ano, levels = c(2024, 2025)))

top_ufs <- ufs_anuais |>
  filter(valor_contratado > 0) |>
  group_by(ano) |>
  slice_max(valor_contratado, n = 3, with_ties = FALSE) |>
  arrange(ano, desc(valor_contratado)) |>
  mutate(rank = row_number()) |>
  ungroup()

top_pontos <- st_point_on_surface(top_ufs)
coords_top <- st_coordinates(top_pontos)
chamadas_top <- top_pontos |>
  st_drop_geometry() |>
  mutate(
    x_uf = coords_top[, 1],
    y_uf = coords_top[, 2],
    x_label = case_when(
      rotulo_uf == "CE" ~ -30.0,
      rotulo_uf == "MG" ~ -31.0,
      rotulo_uf == "RS" ~ -35.5,
      rotulo_uf == "PR" ~ -35.5,
      rotulo_uf == "SC" ~ -35.5,
      TRUE ~ -31.0
    ),
    y_label = case_when(
      rotulo_uf == "CE" ~ -3.8,
      rotulo_uf == "MG" ~ -18.5,
      rotulo_uf == "RS" ~ -31.0,
      rotulo_uf == "PR" ~ -23.5,
      rotulo_uf == "SC" ~ -27.2,
      TRUE ~ y_uf
    ),
    x_alvo = case_when(
      rotulo_uf == "CE" ~ x_uf + 1.9,
      rotulo_uf == "MG" ~ x_uf + 2.1,
      rotulo_uf == "RS" ~ x_uf + 2.0,
      rotulo_uf == "PR" ~ x_uf + 2.0,
      rotulo_uf == "SC" ~ x_uf + 2.0,
      TRUE ~ x_uf + 1.8
    ),
    y_alvo = case_when(
      rotulo_uf == "CE" ~ y_uf + 0.55,
      rotulo_uf == "MG" ~ y_uf + 0.35,
      rotulo_uf == "PR" ~ y_uf + 0.45,
      rotulo_uf == "SC" ~ y_uf - 0.15,
      rotulo_uf == "RS" ~ y_uf + 0.35,
      TRUE ~ y_uf
    ),
    x_seta = x_label - 0.8,
    y_seta = y_label,
    label = paste0(rotulo_uf, " - ", fmt_dinheiro_curto(valor_contratado))
  )

p <- ggplot(ufs_anuais) +
  geom_sf(aes(fill = valor_contratado), color = "black", linewidth = 0.25) +
  geom_segment(
    data = chamadas_top,
    aes(x = x_seta, y = y_seta, xend = x_alvo, yend = y_alvo),
    inherit.aes = FALSE,
    color = "black",
    linewidth = 0.45,
    arrow = arrow(length = unit(2.2, "mm"), type = "closed")
  ) +
  geom_label(
    data = chamadas_top,
    aes(x = x_label, y = y_label, label = label),
    inherit.aes = FALSE,
    color = "black",
    fill = "white",
    linewidth = 0.25,
    label.padding = unit(2.2, "mm"),
    fontface = "bold",
    size = 3
  ) +
  geom_sf_text(aes(label = rotulo_uf), size = 2.7, color = "black", lineheight = 0.85) +
  facet_wrap(~ano, nrow = 1, scales = "fixed") +
  coord_sf(clip = "off") +
  scale_fill_gradient(
    low = "#E8EEF9",
    high = cor_azul,
    limits = c(0, limite_mapa),
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
  labs(title = "Valor contratado por UF", fill = "Valor") +
  tema_fust() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.background = element_rect(fill = "transparent", color = NA),
    legend.background = element_rect(fill = "transparent", color = NA),
    legend.box.background = element_rect(fill = "transparent", color = NA),
    panel.grid = element_blank(),
    legend.position = "bottom",
    strip.background = element_rect(fill = "transparent", color = NA),
    strip.text = element_text(color = cor_texto, face = "bold", size = 12),
    plot.margin = margin(18, 120, 18, 16)
  )

salvar_figura(p, "map_valor_contratado_por_uf.png", width = 13, height = 6.6)
