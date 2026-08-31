# ============================================================================
# ANÁLISE DE CUSTO-BENEFÍCIO — FUST / BNDES
# Mensuração da extensão de fibra óptica
#
# O que este script faz:
#   (1) reproduz, a partir das premissas descritas no produto 3, 
#       o Valor Presente dos BENEFÍCIOS e dos CUSTOS, além do 
#       VPL e a Razão Custo-Benefício (RCB); e
#   (2) gera as tabelas finais formatadas.
#
# Autoria: LEAP/BNDES
#
# ============================================================================

## ---------------------------------------------------------------------------
# 0) PACOTES ####
## ---------------------------------------------------------------------------

library(gt)

# Formata um número em reais no padrão brasileiro
fmt_brl <- function(x) {
  paste0("R$ ", formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
}

## ---------------------------------------------------------------------------
# 1) PREMISSAS ####
## ---------------------------------------------------------------------------
data_focal      <- 2024

# --- Benefício -------------------------------------------------------------
custo_unit_km   <- 61763.45      # custo unitário de referência da fibra (R$/km) — CG-FUST/Anatel
horizonte       <- 15            # anos — European Commission (2014)
extensao_2024   <- 902.01        # km — safra 2024
extensao_2025   <- 3799.45       # km — safra 2025

# --- Taxas de desconto (na data focal 2024) --------------------------------
tx_dpmfi        <- 0.1104        # Taxa 1 — DPMFi — Tesouro dez/2024
tx_ntnb         <- 0.0628        # Taxa 2 — NTN-B — Tesouro dez/2024

# --- Custo (subsídio implícito do crédito) ---------------------------------
rg_2024         <- 0.0628        # custo de captação do governo (a.a.) — safra 2024 (NTN-B1)
rg_2025         <- 0.0723        # idem — safra 2025
txfust_2024     <- 0.0201        # taxa FUST média (a.a.) — safra 2024
txfust_2025     <- 0.0262        # idem — safra 2025
desemb_2024     <- 316600000     # desembolso FUST (R$) — safra 2024
desemb_2025     <- 298300000     # idem — safra 2025
carencia        <- 15            # meses
amortizacao     <- 66            # meses (SAC)


## ------------------------------------------------------------------------------
# 2) BENEFÍCIO — CAPEX evitado distribuído pela Soma dos Dígitos dos Anos (SDA) ####
## ------------------------------------------------------------------------------
# Valor bruto (estoque) de cada safra = extensão (km) × custo unitário (R$/km).
valor_bruto_2024 <- extensao_2024 * custo_unit_km
valor_bruto_2025 <- extensao_2025 * custo_unit_km

# Soma dos dígitos: 1+2+...+H = H(H+1)/2. Os pesos decrescem linearmente:
#   ano 1 = H/SD, ano 2 = (H-1)/SD, ... , ano H = 1/SD  (somam 1).
soma_digitos <- horizonte * (horizonte + 1) / 2          # 120 para H = 15
n            <- 1:horizonte                               # índice do ano da safra (1..15)
peso_sda     <- (horizonte - n + 1) / soma_digitos        # (16-n)/120

# Desconto: a safra 2024 começa na data focal (t = 0..14);
#           a safra 2025 começa 1 ano depois (t = 1..15) — defasagem.
vp_2024_628  <- sum(valor_bruto_2024 * peso_sda / (1 + tx_ntnb )^(n - 1))
vp_2024_1104 <- sum(valor_bruto_2024 * peso_sda / (1 + tx_dpmfi)^(n - 1))
vp_2025_628  <- sum(valor_bruto_2025 * peso_sda / (1 + tx_ntnb )^n)
vp_2025_1104 <- sum(valor_bruto_2025 * peso_sda / (1 + tx_dpmfi)^n)

VPTB_628  <- vp_2024_628  + vp_2025_628       # VP total dos benefícios (6,28%)
VPTB_1104 <- vp_2024_1104 + vp_2025_1104      # VP total dos benefícios (11,04%)

# --- fluxo por ano-calendário (para tabela e transparência da SDA) ---------
anos <- data_focal:(data_focal + horizonte)              # 2024..2039
t    <- anos - data_focal                                 # 0..15
# peso de cada safra em cada ano-calendário (a SDA de cada safra começa no seu 1º ano)
w24  <- ifelse(anos >= 2024 & anos <= 2038, (horizonte - (anos - 2023) + 1) / soma_digitos, 0)
w25  <- ifelse(anos >= 2025 & anos <= 2039, (horizonte - (anos - 2024) + 1) / soma_digitos, 0)
bd24 <- valor_bruto_2024 * w24                            # benefício distribuído safra 2024
bd25 <- valor_bruto_2025 * w25                            # benefício distribuído safra 2025
fluxo_benef <- data.frame(
  Ano         = anos,
  Benef_dist  = bd24 + bd25,
  VP_1104     = bd24/(1 + tx_dpmfi)^t + bd25/(1 + tx_dpmfi)^t,
  VP_628      = bd24/(1 + tx_ntnb )^t + bd25/(1 + tx_ntnb )^t
)


## -----------------------------------------------------------------------------
# 3) CUSTO — subsídio implícito do crédito (SAC), trazido à data focal 2024 ####
## -----------------------------------------------------------------------------

spread_m_2024 <- (1 + (rg_2024 - txfust_2024))^(1/12) - 1
spread_m_2025 <- (1 + (rg_2025 - txfust_2025))^(1/12) - 1
i_m_ntnb      <- (1 + tx_ntnb )^(1/12) - 1
i_m_dpmfi     <- (1 + tx_dpmfi)^(1/12) - 1

# subsídio mensal de uma safra pelo SAC (carência + amortização)
fluxo_custo_safra <- function(safra, desembolso, spread_m, carencia, amortizacao, defasagem_anos) {
  n     <- carencia + amortizacao
  amort <- c(rep(0, carencia), rep(desembolso / amortizacao, amortizacao))
  sd    <- numeric(n); sd[1] <- desembolso
  for (k in 2:n) sd[k] <- sd[k - 1] - amort[k - 1]
  mes      <- 1:n
  subsidio <- spread_m * sd
  data.frame(
    Safra = safra, Mes = mes, Amortizacao = amort, Saldo_Devedor = sd,
    Spread_mensal = spread_m, Subsidio = subsidio,
    VP_1104 = subsidio / (1 + i_m_dpmfi)^mes / (1 + tx_dpmfi)^defasagem_anos,
    VP_628  = subsidio / (1 + i_m_ntnb )^mes / (1 + tx_ntnb )^defasagem_anos
  )
}

custo_2024  <- fluxo_custo_safra("2024", desemb_2024, spread_m_2024, carencia, amortizacao, 0)
custo_2025  <- fluxo_custo_safra("2025", desemb_2025, spread_m_2025, carencia, amortizacao, 1)

fluxo_custo <- rbind(custo_2024, custo_2025)

VPC_2024_1104 <- sum(custo_2024$VP_1104); VPC_2024_628 <- sum(custo_2024$VP_628)
VPC_2025_1104 <- sum(custo_2025$VP_1104); VPC_2025_628 <- sum(custo_2025$VP_628)

VPTC_628  <- VPC_2024_628  + VPC_2025_628
VPTC_1104 <- VPC_2024_1104 + VPC_2025_1104

SI_nom_2024 <- sum(custo_2024$Subsidio)
SI_nom_2025 <- sum(custo_2025$Subsidio)


## ---------------------------------------------------------------------------
# 4) RESULTADO — VPL e RCB nos dois cenários de taxa ####
## ---------------------------------------------------------------------------
VPL_628  <- VPTB_628  - VPTC_628 ;  RCB_628  <- VPTB_628  / VPTC_628
VPL_1104 <- VPTB_1104 - VPTC_1104;  RCB_1104 <- VPTB_1104 / VPTC_1104

cat("\n================ RESULTADO — FIBRA ÓPTICA ================\n")
cat(sprintf("  6,28%%  -> VP benef %s | VP custo %s | VPL %s | RCB %.2f\n",
            fmt_brl(VPTB_628),  fmt_brl(VPTC_628),  fmt_brl(VPL_628),  RCB_628))
cat(sprintf("  11,04%% -> VP benef %s | VP custo %s | VPL %s | RCB %.2f\n",
            fmt_brl(VPTB_1104), fmt_brl(VPTC_1104), fmt_brl(VPL_1104), RCB_1104))
cat("=========================================================\n\n")


## ---------------------------------------------------------------------------
# 5) TABELAS FINAIS (gt) ####
## ---------------------------------------------------------------------------
cor_cab <- "#2F5496"   # azul institucional

# estilo comum (usa tab_style() em vez de tab_options() p/ compatibilidade)
estilo <- function(g) {
  g |>
    opt_table_font(font = "Arial") |>
    tab_style(
      style = list(cell_fill(color = cor_cab),
                   cell_text(color = "white", weight = "bold")),
      locations = cells_column_labels()
    ) |>
    opt_row_striping()
}

## --- 5.1 Premissas #### ---------------------------------------------------------
tab_premissas <- data.frame(
  Parametro = c("Data focal", "Custo unitário da fibra", "Horizonte",
                "Distribuição temporal", "Extensão - safra 2024",
                "Extensão - safra 2025", "Taxa 1 - DPMFi", "Taxa 2 - NTN-B"),
  Valor = c(as.character(data_focal), paste0(fmt_brl(custo_unit_km), "/km"),
            paste(horizonte, "anos"), "Soma dos Dígitos dos Anos (SDA)",
            paste(formatC(extensao_2024, format = "f", digits = 2, big.mark = ".", decimal.mark = ","), "km"),
            paste(formatC(extensao_2025, format = "f", digits = 2, big.mark = ".", decimal.mark = ","), "km"),
            "11,04% a.a.", "6,28% a.a."),
  Fonte = c("Termo de Referência", "CG-FUST / Anatel", "European Commission (2014)",
            "MCASP (2024)", "Base interna BNDES/FUST", "Base interna BNDES/FUST",
            "Tesouro - dez/2024", "Tesouro - dez/2024"),
  stringsAsFactors = FALSE
)
gt_premissas <- gt(tab_premissas) |>
  cols_label(Parametro = "Parâmetro", Valor = "Valor", Fonte = "Fonte") |>
  tab_header(title = "Premissas metodológicas - Fibra óptica") |>
  estilo()

print(gt_premissas)

## --- 5.2 Fluxo do benefício (SDA) #### ------------------------------------------
tab_fluxo <- rbind(
  fluxo_benef,
  data.frame(Ano = "TOTAL", Benef_dist = sum(fluxo_benef$Benef_dist),
             VP_1104 = sum(fluxo_benef$VP_1104), VP_628 = sum(fluxo_benef$VP_628))
)
gt_fluxo <- gt(tab_fluxo) |>
  cols_label(Benef_dist = "Benefício distribuído", VP_1104 = "VP (11,04%)", VP_628 = "VP (6,28%)") |>
  fmt_currency(columns = c(Benef_dist, VP_1104, VP_628),
               currency = "BRL", sep_mark = ".", dec_mark = ",") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Ano == "TOTAL")) |>
  tab_header(title = "Fluxo do benefício pela SDA (perfil decrescente)") |>
  estilo()

print(gt_fluxo)

## --- 5.3 Resumo dos benefícios #### ---------------------------------------------
tab_rb <- data.frame(
  Safra      = c("2024", "2025", "TOTAL"),
  Extensao   = c(extensao_2024, extensao_2025, extensao_2024 + extensao_2025),
  ValorBruto = c(valor_bruto_2024, valor_bruto_2025, valor_bruto_2024 + valor_bruto_2025),
  VP_1104    = c(vp_2024_1104, vp_2025_1104, VPTB_1104),
  VP_628     = c(vp_2024_628,  vp_2025_628,  VPTB_628),
  stringsAsFactors = FALSE
)
gt_rb <- gt(tab_rb) |>
  cols_label(Extensao = "Extensão (km)", ValorBruto = "Valor bruto (evitado)",
             VP_1104 = "VP (11,04%)", VP_628 = "VP (6,28%)") |>
  fmt_number(columns = Extensao, sep_mark = ".", dec_mark = ",", decimals = 2) |>
  fmt_currency(columns = c(ValorBruto, VP_1104, VP_628),
               currency = "BRL", sep_mark = ".", dec_mark = ",") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Safra == "TOTAL")) |>
  tab_header(title = "Resumo - Valor Presente dos Benefícios") |>
  estilo()

print(gt_rb)

## --- 5.4 Tabela do fluxo mensal do custo, agrupada por safra #### ---
gt_fluxo_custo <- gt(fluxo_custo, groupname_col = "Safra") |>
  cols_label(Mes = "Mês", Amortizacao = "Amortização", Saldo_Devedor = "Saldo devedor",
             Spread_mensal = "Spread (a.m.)", Subsidio = "Subsídio mensal",
             VP_1104 = "VP (11,04%)", VP_628 = "VP (6,28%)") |>
  fmt_currency(columns = c(Amortizacao, Saldo_Devedor, Subsidio, VP_1104, VP_628),
               currency = "BRL", sep_mark = ".", dec_mark = ",") |>
  fmt_number(columns = Spread_mensal, decimals = 6, dec_mark = ",") |>
  tab_header(title = "Fluxo mensal do custo - subsídio implícito (SAC)") |>
  estilo()

print(gt_fluxo_custo)

## --- 5.4 Resumo dos custos #### -------------------------------------------------
tab_rc <- data.frame(
  Safra        = c("2024", "2025", "TOTAL"),
  Desembolso   = c(desemb_2024, desemb_2025, desemb_2024 + desemb_2025),
  TaxaFUST     = c(txfust_2024, txfust_2025, NA),
  Spread       = c(rg_2024 - txfust_2024, rg_2025 - txfust_2025, NA),
  Subsidio_nom = c(SI_nom_2024, SI_nom_2025, SI_nom_2024 + SI_nom_2025),
  VP_1104      = c(VPC_2024_1104, VPC_2025_1104, VPTC_1104),
  VP_628       = c(VPC_2024_628,  VPC_2025_628,  VPTC_628),
  stringsAsFactors = FALSE
)
gt_rc <- gt(tab_rc) |>
  cols_label(Desembolso = "Valor desembolsado", TaxaFUST = "Taxa FUST (média)",
             Spread = "Spread anual", Subsidio_nom = "Subsídio nominal",
             VP_1104 = "VP (11,04%)", VP_628 = "VP (6,28%)") |>
  fmt_currency(columns = c(Desembolso, Subsidio_nom, VP_1104, VP_628),
               currency = "BRL", sep_mark = ".", dec_mark = ",") |>
  fmt_percent(columns = c(TaxaFUST, Spread), decimals = 2, sep_mark = ".", dec_mark = ",") |>
  sub_missing(columns = c(TaxaFUST, Spread), missing_text = "–") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Safra == "TOTAL")) |>
  tab_header(title = "Resumo - Valor Presente dos Custos (subsídio implícito)") |>
  estilo()

print(gt_rc)

## --- 5.5 Resultado (VPL e RCB) #### ---------------------------------------------
tab_res <- data.frame(
  Indicador = c("VP dos benefícios (VPBT)", "VP dos custos (VPCT)",
                "VPL (benefício - custo)", "RCB (benefício / custo)"),
  Cenario_628  = c(fmt_brl(VPTB_628),  fmt_brl(VPTC_628),  fmt_brl(VPL_628),
                   formatC(RCB_628,  format = "f", digits = 2, decimal.mark = ",")),
  Cenario_1104 = c(fmt_brl(VPTB_1104), fmt_brl(VPTC_1104), fmt_brl(VPL_1104),
                   formatC(RCB_1104, format = "f", digits = 2, decimal.mark = ",")),
  stringsAsFactors = FALSE
)
gt_res <- gt(tab_res) |>
  cols_label(Indicador = "", Cenario_628 = "Cenário 6,28% (NTN-B)",
             Cenario_1104 = "Cenário 11,04% (DPMFi)") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Indicador %in%
                                     c("VPL (benefício - custo)", "RCB (benefício / custo)"))) |>
  tab_header(title = "Resultado da Análise Custo-Benefício - Fibra óptica",
             subtitle = "Benefício = distribuído por SDA · Custo = subsídio implícito") |>
  estilo()

print(gt_res)

## ---------------------------------------------------------------------------
# 6) SALVAR ####
## ---------------------------------------------------------------------------

dir.create("tabelas_fibra", showWarnings = FALSE)
salvar <- function(g, nome) {
  gtsave(g, file.path("tabelas_fibra", paste0(nome, ".html")))
  tryCatch(
    gtsave(g, file.path("tabelas_fibra", paste0(nome, ".png"))),
    error = function(e) message("PNG de '", nome,
                                "' requer webshot2/chromote + Chrome; HTML salvo.")
  )
}
salvar(gt_premissas, "01_premissas")
salvar(gt_fluxo,     "02_fluxo_beneficio_SDA")
salvar(gt_rb,        "03_resumo_beneficios")
salvar(gt_fluxo_custo, "04_gt_fluxo_custo")
salvar(gt_rc,        "05_resumo_custos")
salvar(gt_res,       "06_resultado")

message("Tabelas geradas em: ", normalizePath("tabelas_fibra"))
# ============================================================================
# FIM
# ============================================================================
