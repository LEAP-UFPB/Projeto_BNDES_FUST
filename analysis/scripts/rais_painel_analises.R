args <- commandArgs(trailingOnly = FALSE)
script_path <- normalizePath(sub("^--file=", "", args[grepl("^--file=", args)]))
source(file.path(dirname(script_path), "_common.R"))
gerar_mapas <- !("--sem-mapas" %in% commandArgs(trailingOnly = TRUE))

pacotes_rais <- c("sf", "writexl")
faltando_rais <- setdiff(pacotes_rais, rownames(installed.packages()))
if (length(faltando_rais) > 0) {
  install.packages(faltando_rais)
}

suppressPackageStartupMessages({
  library(sf)
  library(writexl)
})

rais_out_dir <- file.path(output_dir, "rais_panel")
dir.create(rais_out_dir, recursive = TRUE, showWarnings = FALSE)

arquivo_painel <- file.path(data_dir, "Painel_CNPJ_FUST_RAIS_2018_2024.csv")
arquivo_municipios <- file.path(data_dir, "mapas", "BR_Municipios_2021.shp")

limpar_digitos <- function(x) {
  x <- str_replace_all(as.character(x), "\\D", "")
  x[x == ""] <- NA_character_
  x
}

cnpj_raiz <- function(x) {
  str_sub(str_pad(limpar_digitos(x), 14, pad = "0"), 1, 8)
}

media_ponderada <- function(x, peso) {
  ok <- !is.na(x) & !is.na(peso) & peso > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  weighted.mean(x[ok], peso[ok])
}

subsetor_ibge_label <- function(code) {
  labels <- c(
    "01" = "Extrativa mineral",
    "02" = "Produtos minerais não metálicos",
    "03" = "Indústria metalúrgica",
    "04" = "Indústria mecânica",
    "05" = "Elétrico e comunicações",
    "06" = "Material de transporte",
    "07" = "Madeira e mobiliário",
    "08" = "Papel, papelão, editorial e gráfica",
    "09" = "Borracha, fumo e couros",
    "10" = "Indústria química",
    "11" = "Indústria têxtil",
    "12" = "Indústria de calçados",
    "13" = "Alimentos e bebidas",
    "14" = "Serviços de utilidade pública",
    "15" = "Construção civil",
    "16" = "Comércio varejista",
    "17" = "Comércio atacadista",
    "18" = "Instituições financeiras",
    "19" = "Administração técnica profissional",
    "20" = "Transporte e comunicações",
    "21" = "Alojamento, alimentação e reparação",
    "22" = "Médico, odontológico e veterinário",
    "23" = "Ensino",
    "24" = "Administração pública",
    "25" = "Agricultura"
  )
  unname(labels[str_pad(as.character(code), 2, pad = "0")])
}

salvar_csv <- function(df, nome, nome_secao = NULL) {
  nome_saida <- if (is.null(nome_secao)) nome else nome_secao
  readr::write_excel_csv2(df, file.path(rais_out_dir, nome_saida), na = "")
}

salvar_figura_rais <- function(
  plot,
  nome,
  nome_secao,
  width = 9,
  height = 5,
  dpi = 220
) {
  nome_saida <- if (is.null(nome_secao)) nome else nome_secao
  ggsave(
    file.path(rais_out_dir, nome_saida),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "transparent"
  )
}

painel <- read_csv(
  arquivo_painel,
  col_types = cols(
    .default = col_guess(),
    id_empresa_painel = col_character(),
    cnpj_raiz = col_character(),
    cnpj_matriz_rais = col_character(),
    cnpj_fust = col_character()
  ),
  show_col_types = FALSE
) |>
  mutate(
    ano = as.integer(ano),
    id_empresa_painel = as.character(id_empresa_painel),
    cnpj_raiz = na_if(str_pad(limpar_digitos(cnpj_raiz), 8, pad = "0"), "00000000"),
    cliente_norm = normalizar_texto(cliente_fust),
    encontrado_rais = as.integer(coalesce(encontrado_rais, 0)),
    status_encontro = if_else(encontrado_rais == 1, "Encontrada", "Nao encontrada"),
    uf_fust = if_else(str_to_upper(uf_fust) %in% ufs_brasil, str_to_upper(uf_fust), NA_character_),
    municipio_fust_norm = normalizar_texto(municipio_fust)
  )

base_fust <- ler_base() |>
  mutate(
    cnpj_raiz = cnpj_raiz(cnpj),
    cliente_norm = normalizar_texto(cliente),
    tipo_operacao = case_when(
      str_detect(normalizar_texto(tipo_operacao), "INDIRETA") ~ "Indireta",
      str_detect(normalizar_texto(origem_detalhada), "INDIRETA") ~ "Indireta",
      TRUE ~ "Direta"
    )
  ) |>
  select(cnpj_raiz, cliente_norm, tipo_operacao) |>
  distinct()

modalidade_cnpj <- painel |>
  filter(!is.na(cnpj_raiz)) |>
  select(id_empresa_painel, cnpj_raiz) |>
  distinct() |>
  inner_join(
    base_fust |> filter(!is.na(cnpj_raiz)) |> select(cnpj_raiz, tipo_operacao) |> distinct(),
    by = "cnpj_raiz"
  ) |>
  select(id_empresa_painel, tipo_operacao)

modalidade_cliente <- painel |>
  select(id_empresa_painel, cliente_norm) |>
  distinct() |>
  anti_join(modalidade_cnpj |> distinct(id_empresa_painel), by = "id_empresa_painel") |>
  inner_join(
    base_fust |> filter(!is.na(cliente_norm)) |> select(cliente_norm, tipo_operacao) |> distinct(),
    by = "cliente_norm"
  ) |>
  select(id_empresa_painel, tipo_operacao)

modalidades <- bind_rows(modalidade_cnpj, modalidade_cliente) |>
  distinct() |>
  mutate(tipo_operacao = factor(tipo_operacao, levels = c("Direta", "Indireta")))

painel_modalidade <- painel |>
  left_join(modalidades, by = "id_empresa_painel", relationship = "many-to-many") |>
  mutate(
    tipo_operacao = replace_na(as.character(tipo_operacao), "Nao identificado"),
    tipo_operacao = factor(tipo_operacao, levels = c("Direta", "Indireta", "Nao identificado"))
  )

empresas_status <- painel |>
  group_by(id_empresa_painel, cliente_fust) |>
  summarise(
    anos_no_painel = n_distinct(ano),
    anos_encontrados = n_distinct(ano[encontrado_rais == 1]),
    anos_nao_encontrados = n_distinct(ano[encontrado_rais == 0]),
    .groups = "drop"
  ) |>
  mutate(
    status_empresa = case_when(
      anos_encontrados == 0 ~ "Nunca encontrada",
      anos_nao_encontrados == 0 ~ "Encontrada em todos os anos",
      TRUE ~ "Encontrada em parte dos anos"
    )
  )

tab_empresas_encontro_geral <- empresas_status |>
  count(status_empresa, name = "empresas") |>
  mutate(total_empresas_painel = sum(empresas)) |>
  arrange(match(status_empresa, c(
    "Encontrada em todos os anos",
    "Encontrada em parte dos anos",
    "Nunca encontrada"
  )))

tab_empresas_encontro_cnpj_raiz <- painel |>
  filter(!is.na(cnpj_raiz)) |>
  group_by(cnpj_raiz) |>
  summarise(
    anos_no_painel = n_distinct(ano),
    anos_encontrados = n_distinct(ano[encontrado_rais == 1]),
    anos_nao_encontrados = n_distinct(ano[encontrado_rais == 0]),
    .groups = "drop"
  ) |>
  mutate(
    status_empresa = case_when(
      anos_encontrados == 0 ~ "Nunca encontrada",
      anos_nao_encontrados == 0 ~ "Encontrada em todos os anos",
      TRUE ~ "Encontrada em parte dos anos"
    )
  ) |>
  count(status_empresa, name = "empresas_com_cnpj_raiz") |>
  mutate(total_empresas_com_cnpj_raiz = sum(empresas_com_cnpj_raiz)) |>
  arrange(match(status_empresa, c(
    "Encontrada em todos os anos",
    "Encontrada em parte dos anos",
    "Nunca encontrada"
  )))

tab_empresas_encontro_por_ano_modalidade <- painel_modalidade |>
  group_by(ano, tipo_operacao, status_encontro) |>
  summarise(empresas = n_distinct(id_empresa_painel), .groups = "drop") |>
  pivot_wider(names_from = status_encontro, values_from = empresas, values_fill = 0) |>
  mutate(total_empresas = Encontrada + `Nao encontrada`) |>
  arrange(ano, tipo_operacao)

tab_resumo_ano_modalidade <- painel_modalidade |>
  group_by(ano, tipo_operacao) |>
  summarise(
    empresas = n_distinct(id_empresa_painel),
    empresas_encontradas = n_distinct(id_empresa_painel[encontrado_rais == 1]),
    empresas_nao_encontradas = n_distinct(id_empresa_painel[encontrado_rais == 0]),
    media_salarial = media_ponderada(rendimento_medio, total_empregados),
    total_empregados = sum(total_empregados, na.rm = TRUE),
    novos_empregos = sum(novos_empregos, na.rm = TRUE),
    media_massa_salarial_empresa = mean(massa_salarial_media, na.rm = TRUE),
    massa_salarial_total = sum(massa_salarial_dezembro, na.rm = TRUE),
    desligamentos = sum(desligamentos, na.rm = TRUE),
    saldo_empregos = sum(saldo_empregos, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    media_salarial = round(media_salarial, 2),
    media_massa_salarial_empresa = round(media_massa_salarial_empresa, 2),
    massa_salarial_total = round(massa_salarial_total, 2)
  ) |>
  arrange(ano, tipo_operacao)

tab_empregos_subsetor <- painel_modalidade |>
  select(id_empresa_painel, ano, tipo_operacao, starts_with("empregados_subsetor_ibge_")) |>
  pivot_longer(
    starts_with("empregados_subsetor_ibge_"),
    names_to = "subsetor_codigo",
    values_to = "total_empregados"
  ) |>
  mutate(
    subsetor_codigo = str_extract(subsetor_codigo, "\\d+$"),
    subsetor = coalesce(subsetor_ibge_label(subsetor_codigo), "Nao informado")
  ) |>
  group_by(ano, tipo_operacao, subsetor_codigo, subsetor) |>
  summarise(total_empregados = sum(total_empregados, na.rm = TRUE), .groups = "drop") |>
  filter(total_empregados > 0) |>
  arrange(ano, tipo_operacao, desc(total_empregados))

colunas_uf_empregados <- names(painel_modalidade)[
  str_detect(names(painel_modalidade), "^empregados_uf_[A-Z]{2}$")
]

tab_empregados_uf <- painel_modalidade |>
  select(id_empresa_painel, ano, tipo_operacao, all_of(colunas_uf_empregados)) |>
  pivot_longer(
    starts_with("empregados_uf_"),
    names_to = "uf",
    values_to = "total_empregados"
  ) |>
  mutate(uf = str_remove(uf, "^empregados_uf_")) |>
  group_by(ano, tipo_operacao, uf) |>
  summarise(total_empregados = sum(total_empregados, na.rm = TRUE), .groups = "drop") |>
  filter(total_empregados > 0) |>
  arrange(ano, tipo_operacao, desc(total_empregados))

tab_empregados_municipio <- painel_modalidade |>
  filter(!is.na(uf_fust), !is.na(municipio_fust_norm), municipio_fust_norm != "SEM MUNICIPIO") |>
  group_by(ano, tipo_operacao, uf_fust, municipio_fust, municipio_fust_norm) |>
  summarise(
    empresas = n_distinct(id_empresa_painel),
    total_empregados = sum(total_empregados, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(total_empregados > 0) |>
  arrange(ano, tipo_operacao, desc(total_empregados))

salvar_csv(
  tab_empresas_encontro_geral,
  "tab_empresas_encontro_geral.csv",
  "2.1 Cobertura das empresas FUST na RAIS - resumo geral.csv"
)
salvar_csv(
  tab_empresas_encontro_cnpj_raiz,
  "tab_empresas_encontro_cnpj_raiz.csv",
  "2.1 Cobertura das empresas FUST na RAIS - resumo por CNPJ raiz.csv"
)
salvar_csv(
  tab_empresas_encontro_por_ano_modalidade,
  "tab_empresas_encontro_por_ano_modalidade.csv",
  "2.1 Cobertura das empresas FUST na RAIS - evolução anual por modalidade.csv"
)
salvar_csv(
  tab_resumo_ano_modalidade,
  "tab_resumo_ano_modalidade.csv",
  "2.2 Evolução do emprego e remuneração - indicadores anuais por modalidade.csv"
)
salvar_csv(
  tab_empregos_subsetor,
  "tab_empregos_por_subsetor.csv",
  "2.3 Distribuição setorial do emprego - tabela anual por subsetor.csv"
)
salvar_csv(
  tab_empregados_uf,
  "tab_total_empregados_por_uf.csv",
  "2.4 Distribuição geográfica do emprego - tabela anual por UF.csv"
)
salvar_csv(
  tab_empregados_municipio,
  "tab_total_empregados_por_municipio_fust.csv",
  "2.4 Distribuição geográfica do emprego - tabela anual por município.csv"
)

paleta_modalidade_rais <- c(
  "Direta" = cor_azul,
  "Indireta" = cor_verde,
  "Nao identificado" = cor_azul_claro
)

status_plot <- tab_empresas_encontro_geral |>
  mutate(
    status_empresa = factor(
      status_empresa,
      levels = c(
        "Encontrada em todos os anos",
        "Encontrada em parte dos anos",
        "Nunca encontrada"
      )
    ),
    proporcao = empresas / sum(empresas),
    rotulo = paste0(
      fmt_int(empresas),
      "\n",
      percent(proporcao, accuracy = 0.1, decimal.mark = ",")
    )
  )

p_cobertura_geral <- ggplot(
  status_plot,
  aes(x = 2, y = proporcao, fill = status_empresa)
) +
  geom_col(width = 1, color = cor_fundo, linewidth = 0.8) +
  geom_text(
    aes(label = rotulo),
    position = position_stack(vjust = 0.5),
    color = "black",
    fontface = "bold",
    size = 3.7,
    lineheight = 0.95
  ) +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(
    values = c(
      "Encontrada em todos os anos" = cor_verde,
      "Encontrada em parte dos anos" = cor_azul,
      "Nunca encontrada" = cor_azul_claro
    ),
    drop = FALSE
  ) +
  labs(fill = NULL) +
  tema_donut_escuro()

salvar_figura_rais(
  p_cobertura_geral,
  "fig_cobertura_empresas_rais.png",
  "2.1 Cobertura das empresas FUST na RAIS - situação das empresas no período.png",
  width = 8.5,
  height = 5
)

cobertura_ano_plot <- tab_empresas_encontro_por_ano_modalidade |>
  mutate(
    taxa_encontro = if_else(total_empresas > 0, Encontrada / total_empresas, 0),
    tipo_operacao = factor(
      as.character(tipo_operacao),
      levels = c("Direta", "Indireta", "Nao identificado")
    )
  )

p_cobertura_ano <- ggplot(
  cobertura_ano_plot,
  aes(x = ano, y = taxa_encontro, color = tipo_operacao, group = tipo_operacao)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(
      label = percent(taxa_encontro, accuracy = 0.1, decimal.mark = ","),
      vjust = if_else(as.character(tipo_operacao) == "Direta", -0.9, 1.7)
    ),
    color = "black",
    fontface = "bold",
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_x_continuous(breaks = sort(unique(cobertura_ano_plot$ano))) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = percent_format(accuracy = 1, decimal.mark = ","),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(color = NULL) +
  tema_fust_escuro()

salvar_figura_rais(
  p_cobertura_ano,
  "fig_taxa_encontro_rais_por_ano.png",
  "2.1 Cobertura das empresas FUST na RAIS - taxa de encontro por ano e modalidade.png",
  width = 9,
  height = 5
)

emprego_plot <- tab_resumo_ano_modalidade |>
  mutate(
    tipo_operacao = factor(
      as.character(tipo_operacao),
      levels = c("Direta", "Indireta", "Nao identificado")
    )
  )

p_empregados <- ggplot(
  emprego_plot,
  aes(x = ano, y = total_empregados, color = tipo_operacao, group = tipo_operacao)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = fmt_int(total_empregados)),
    vjust = -0.9,
    color = "black",
    fontface = "bold",
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_x_continuous(breaks = sort(unique(emprego_plot$ano))) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(color = NULL) +
  tema_fust_escuro()

salvar_figura_rais(
  p_empregados,
  "fig_total_empregados_por_ano.png",
  "2.2 Evolução do emprego e remuneração - total de empregados por ano.png",
  width = 9,
  height = 5
)

movimentacao_plot <- emprego_plot |>
  select(ano, tipo_operacao, novos_empregos, desligamentos, saldo_empregos) |>
  pivot_longer(
    c(novos_empregos, desligamentos, saldo_empregos),
    names_to = "indicador",
    values_to = "valor"
  ) |>
  mutate(
    indicador = factor(
      indicador,
      levels = c("novos_empregos", "desligamentos", "saldo_empregos"),
      labels = c("Novos empregos", "Desligamentos", "Saldo de empregos")
    )
  )

p_movimentacao <- ggplot(
  movimentacao_plot,
  aes(x = ano, y = valor, color = tipo_operacao, group = tipo_operacao)
) +
  geom_hline(yintercept = 0, color = cor_grade, linewidth = 0.6) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.4) +
  facet_wrap(~indicador, ncol = 1, scales = "fixed") +
  scale_color_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_x_continuous(breaks = sort(unique(movimentacao_plot$ano))) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0.08, 0.12))
  ) +
  labs(color = NULL) +
  tema_fust_escuro()

salvar_figura_rais(
  p_movimentacao,
  "fig_movimentacao_empregos_por_ano.png",
  "2.2 Evolução do emprego e remuneração - admissões desligamentos e saldo.png",
  width = 9,
  height = 8.5
)

p_salario <- ggplot(
  emprego_plot,
  aes(x = ano, y = media_salarial, color = tipo_operacao, group = tipo_operacao)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = fmt_dinheiro_curto(media_salarial)),
    vjust = -0.9,
    color = "black",
    fontface = "bold",
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_x_continuous(breaks = sort(unique(emprego_plot$ano))) +
  scale_y_continuous(
    labels = fmt_dinheiro_curto,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(color = NULL) +
  tema_fust_escuro()

salvar_figura_rais(
  p_salario,
  "fig_rendimento_medio_por_ano.png",
  "2.2 Evolução do emprego e remuneração - rendimento médio por ano.png",
  width = 9,
  height = 5
)

p_massa_salarial <- ggplot(
  emprego_plot,
  aes(x = ano, y = massa_salarial_total, color = tipo_operacao, group = tipo_operacao)
) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = fmt_dinheiro_curto(massa_salarial_total)),
    vjust = -0.9,
    color = "black",
    fontface = "bold",
    size = 3.1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_x_continuous(breaks = sort(unique(emprego_plot$ano))) +
  scale_y_continuous(
    labels = fmt_dinheiro_curto,
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(color = NULL) +
  tema_fust_escuro()

salvar_figura_rais(
  p_massa_salarial,
  "fig_massa_salarial_por_ano.png",
  "2.2 Evolução do emprego e remuneração - massa salarial por ano.png",
  width = 9,
  height = 5
)

ano_mais_recente <- max(tab_empregos_subsetor$ano, na.rm = TRUE)

top_subsetores <- tab_empregos_subsetor |>
  filter(ano == ano_mais_recente) |>
  group_by(subsetor) |>
  summarise(total = sum(total_empregados, na.rm = TRUE), .groups = "drop") |>
  slice_max(total, n = 10, with_ties = FALSE) |>
  arrange(total) |>
  pull(subsetor)

subsetor_plot <- tab_empregos_subsetor |>
  filter(ano == ano_mais_recente, subsetor %in% top_subsetores) |>
  mutate(subsetor = factor(subsetor, levels = top_subsetores))

subsetor_totais <- subsetor_plot |>
  group_by(subsetor) |>
  summarise(total = sum(total_empregados, na.rm = TRUE), .groups = "drop")

p_subsetor <- ggplot(
  subsetor_plot,
  aes(x = subsetor, y = total_empregados, fill = tipo_operacao)
) +
  geom_col(width = 0.65) +
  geom_text(
    data = subsetor_totais,
    aes(x = subsetor, y = total, label = fmt_int(total)),
    inherit.aes = FALSE,
    hjust = -0.12,
    color = "black",
    fontface = "bold",
    size = 3.1
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(18, 60, 18, 16)
  )

salvar_figura_rais(
  p_subsetor,
  "fig_top_subsetores_emprego.png",
  paste0(
    "2.3 Distribuição setorial do emprego - principais subsetores em ",
    ano_mais_recente,
    ".png"
  ),
  width = 10,
  height = 6.5
)

ano_uf_mais_recente <- max(tab_empregados_uf$ano, na.rm = TRUE)

top_ufs <- tab_empregados_uf |>
  filter(ano == ano_uf_mais_recente) |>
  group_by(uf) |>
  summarise(total = sum(total_empregados, na.rm = TRUE), .groups = "drop") |>
  slice_max(total, n = 10, with_ties = FALSE) |>
  arrange(total) |>
  pull(uf)

uf_plot <- tab_empregados_uf |>
  filter(ano == ano_uf_mais_recente, uf %in% top_ufs) |>
  mutate(uf = factor(uf, levels = top_ufs))

uf_totais <- uf_plot |>
  group_by(uf) |>
  summarise(total = sum(total_empregados, na.rm = TRUE), .groups = "drop")

p_uf <- ggplot(
  uf_plot,
  aes(x = uf, y = total_empregados, fill = tipo_operacao)
) +
  geom_col(width = 0.65) +
  geom_text(
    data = uf_totais,
    aes(x = uf, y = total, label = fmt_int(total)),
    inherit.aes = FALSE,
    hjust = -0.12,
    color = "black",
    fontface = "bold",
    size = 3.1
  ) +
  coord_flip(clip = "off") +
  scale_fill_manual(values = paleta_modalidade_rais, drop = TRUE) +
  scale_y_continuous(
    labels = label_number(big.mark = ".", decimal.mark = ","),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(fill = NULL) +
  tema_fust_escuro() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(18, 60, 18, 16)
  )

salvar_figura_rais(
  p_uf,
  "fig_top_ufs_emprego.png",
  paste0(
    "2.4 Distribuição geográfica do emprego - principais UFs em ",
    ano_uf_mais_recente,
    ".png"
  ),
  width = 9,
  height = 6.2
)

write_xlsx(
  list(
    encontro_geral = tab_empresas_encontro_geral,
    encontro_cnpj_raiz = tab_empresas_encontro_cnpj_raiz,
    encontro_ano_modalidade = tab_empresas_encontro_por_ano_modalidade,
    resumo_ano_modalidade = tab_resumo_ano_modalidade,
    empregos_subsetor = tab_empregos_subsetor,
    empregados_uf = tab_empregados_uf,
    empregados_municipio = tab_empregados_municipio
  ),
  file.path(rais_out_dir, "analises_rais_painel_fust.xlsx")
)

if (gerar_mapas && file.exists(arquivo_municipios) && nrow(tab_empregados_municipio) > 0) {
  municipios <- st_read(arquivo_municipios, quiet = TRUE) |>
    mutate(
      uf_fust = SIGLA,
      municipio_fust_norm = normalizar_texto(NM_MUN)
    )

  limite_mapa <- max(tab_empregados_municipio$total_empregados, na.rm = TRUE)

  for (ano_mapa in sort(unique(tab_empregados_municipio$ano))) {
    dados_ano <- tab_empregados_municipio |>
      filter(ano == ano_mapa) |>
      group_by(tipo_operacao, uf_fust, municipio_fust_norm) |>
      summarise(total_empregados = sum(total_empregados, na.rm = TRUE), .groups = "drop")

    mapa_ano <- municipios |>
      left_join(dados_ano, by = c("uf_fust", "municipio_fust_norm")) |>
      mutate(
        tipo_operacao = replace_na(as.character(tipo_operacao), "Sem operacao"),
        total_empregados = replace_na(total_empregados, 0)
      ) |>
      filter(tipo_operacao %in% c("Direta", "Indireta") | total_empregados == 0)

    mapa_ano <- bind_rows(
      mapa_ano |> mutate(tipo_operacao = "Direta") |> filter(total_empregados == 0),
      mapa_ano |> mutate(tipo_operacao = "Indireta") |> filter(total_empregados == 0),
      mapa_ano |> filter(total_empregados > 0)
    ) |>
      group_by(tipo_operacao, CD_MUN) |>
      slice_max(total_empregados, n = 1, with_ties = FALSE) |>
      ungroup() |>
      mutate(tipo_operacao = factor(tipo_operacao, levels = c("Direta", "Indireta")))

    p <- ggplot(mapa_ano) +
      geom_sf(aes(fill = total_empregados), color = NA) +
      facet_wrap(~tipo_operacao, nrow = 1) +
      scale_fill_gradient(
        low = "#E8EEF9",
        high = cor_azul,
        limits = c(0, limite_mapa),
        labels = label_number(big.mark = ".", decimal.mark = ","),
        guide = guide_colorbar(
          direction = "horizontal",
          barheight = unit(2, units = "mm"),
          barwidth = unit(55, units = "mm"),
          draw.ulim = FALSE,
          title.position = "top",
          title.hjust = 0.5,
          label.hjust = 0.5
        )
      ) +
      labs(fill = "Empregados") +
      tema_fust() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom",
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        strip.background = element_rect(fill = "transparent", color = NA),
        strip.text = element_text(color = cor_texto, face = "bold", size = 12)
      )

    salvar_figura_rais(
      p,
      paste0("map_total_empregados_municipio_", ano_mapa, ".png"),
      paste0(
        "2.4 Distribuição geográfica do emprego - mapa municipal de ",
        ano_mapa,
        ".png"
      ),
      width = 12,
      height = 6.6,
      dpi = 300
    )
  }
}
