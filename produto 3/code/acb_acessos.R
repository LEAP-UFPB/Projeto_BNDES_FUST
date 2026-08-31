# ============================================================================
# ANÁLISE DE CUSTO-BENEFÍCIO — FUST / BNDES
# Mensuração do aceso à serviços de banda larga fixa
#
# O que este script faz:
#   (1) reproduz, a partir das premissas descritas no produto 3, 
#       o Valor Presente dos BENEFÍCIOS e dos CUSTOS, além do 
#        VPL e a Razão Custo-Benefício (RCB); e
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
data_focal     <- 2024

# --- Benefício -------------------------------------------------------------
arpu_mensal    <- 90.52          # ARPU banda larga fixa (PPP) — Anatel, Panorama PPPs 4T2024
arpu_anual     <- arpu_mensal * 12
horizonte      <- 15             # anos — European Commission (2014), banda larga 15–20
acessos_2024   <- 40108          # QR FUST / Painel FUST (Anatel)
acessos_2025   <- 4975           # QR FUST / Painel FUST (Anatel)
adicionalidade <- 1.00           # fração dos acessos atribuível ao FUST (<= 1)

# --- Taxas de desconto (na data focal 2024) --------------------------------
tx_dpmfi       <- 0.1104         # Taxa 1 — custo marginal da dívida (DPMFi) — Tesouro dez/2024
tx_ntnb        <- 0.0628         # Taxa 2 — NTN-B — Tesouro dez/2024

# --- Custo (subsídio implícito do crédito) ---------------------------------
rg_2024        <- 0.0628         # custo de captação do governo (a.a.) — safra 2024 (NTN-B1)
rg_2025        <- 0.0723         # idem — safra 2025
txfust_2024    <- 0.0201         # taxa FUST média (a.a.) — safra 2024
txfust_2025    <- 0.0262         # idem — safra 2025
desemb_2024    <- 316600000      # desembolso FUST (R$) — safra 2024
desemb_2025    <- 298300000      # idem — safra 2025
carencia       <- 15             # meses
amortizacao    <- 66             # meses (SAC)


## ------------------------------------------------------------------------------
# 2) BENEFÍCIO — fluxo nivelado "acessos x ARPU anual" ao longo do horizonte ####
## ------------------------------------------------------------------------------
# Cada safra gera o mesmo valor todo ano, por 15 anos, a partir do ano da safra.
#   Safra 2024: anos 2024..2038 (t = 0..14)
#   Safra 2025: anos 2025..2039 (t = 1..15)  -> começa 1 ano depois (defasagem)
anos <- data_focal:(data_focal + horizonte)   # 2024..2039
t    <- anos - data_focal                      # 0..15

ben_anual_2024 <- acessos_2024 * arpu_anual * adicionalidade
ben_anual_2025 <- acessos_2025 * arpu_anual * adicionalidade

# benefício de cada safra em cada ano (zero fora da janela de 15 anos)
ben_2024 <- ifelse(anos >= 2024 & anos <= 2024 + horizonte - 1, ben_anual_2024, 0)
ben_2025 <- ifelse(anos >= 2025 & anos <= 2025 + horizonte - 1, ben_anual_2025, 0)

# desconto anual: valor / (1 + taxa)^t
vp_anual <- function(fluxo, taxa, periodo) fluxo / (1 + taxa)^periodo

fluxo_benef <- data.frame(
  Ano              = anos,
  t                = t,
  Benef_safra_2024 = ben_2024,
  Benef_safra_2025 = ben_2025,
  Benef_total      = ben_2024 + ben_2025,
  VP_2024_1104     = vp_anual(ben_2024, tx_dpmfi, t),
  VP_2025_1104     = vp_anual(ben_2025, tx_dpmfi, t),
  VP_2024_628      = vp_anual(ben_2024, tx_ntnb,  t),
  VP_2025_628      = vp_anual(ben_2025, tx_ntnb,  t)
)

VPB_2024_1104 <- sum(fluxo_benef$VP_2024_1104); VPB_2025_1104 <- sum(fluxo_benef$VP_2025_1104)
VPB_2024_628  <- sum(fluxo_benef$VP_2024_628);  VPB_2025_628  <- sum(fluxo_benef$VP_2025_628)

VPTB_628  <- VPB_2024_628  + VPB_2025_628
VPTB_1104 <- VPB_2024_1104 + VPB_2025_1104


## -----------------------------------------------------------------------------
# 3) CUSTO — subsídio implícito do crédito (SAC), trazido à data focal 2024 ####
## -----------------------------------------------------------------------------

# taxas mensais equivalentes (composição): i_m = (1 + i_a)^(1/12) - 1
spread_m_2024 <- (1 + (rg_2024 - txfust_2024))^(1/12) - 1   # spread mensal safra 2024
spread_m_2025 <- (1 + (rg_2025 - txfust_2025))^(1/12) - 1   # spread mensal safra 2025
i_m_ntnb      <- (1 + tx_ntnb )^(1/12) - 1                  # desconto mensal 6,28%
i_m_dpmfi     <- (1 + tx_dpmfi)^(1/12) - 1                  # desconto mensal 11,04%

# Fluxo mensal do subsídio de uma safra (SAC).
# defasagem_anos = 1 na safra 2025 (traz o fluxo à data focal 2024).
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

fluxo_custo <- rbind(custo_2024, custo_2025)   # 81 meses x 2 safras

VPC_2024_1104 <- sum(custo_2024$VP_1104); VPC_2024_628 <- sum(custo_2024$VP_628)
VPC_2025_1104 <- sum(custo_2025$VP_1104); VPC_2025_628 <- sum(custo_2025$VP_628)

VPTC_628  <- VPC_2024_628  + VPC_2025_628
VPTC_1104 <- VPC_2024_1104 + VPC_2025_1104

SI_nom_2024 <- sum(custo_2024$Subsidio); SI_nom_2025 <- sum(custo_2025$Subsidio)


## ---------------------------------------------------------------------------
# 4) RESULTADO — VPL e RCB nos dois cenários de taxa ####
## ---------------------------------------------------------------------------
VPL_628  <- VPTB_628  - VPTC_628 ;  RCB_628  <- VPTB_628  / VPTC_628
VPL_1104 <- VPTB_1104 - VPTC_1104;  RCB_1104 <- VPTB_1104 / VPTC_1104

cat("\n================ RESULTADO — ACESSO (ARPU) ================\n")
cat(sprintf("  6,28%%  -> VP benef %s | VP custo %s | VPL %s | RCB %.2f\n",
            fmt_brl(VPTB_628),  fmt_brl(VPTC_628),  fmt_brl(VPL_628),  RCB_628))
cat(sprintf("  11,04%% -> VP benef %s | VP custo %s | VPL %s | RCB %.2f\n",
            fmt_brl(VPTB_1104), fmt_brl(VPTC_1104), fmt_brl(VPL_1104), RCB_1104))
cat("=================================================================\n\n")


## ---------------------------------------------------------------------------
# 5) TABELAS FINAIS (gt) ####
## ---------------------------------------------------------------------------
cor_cab <- "#2F5496"   # azul institucional (cabeçalhos)

# estilo comum a todas as tabelas
estilo <- function(g) {
  g |>
    opt_table_font(font = "Arial") |>
    tab_style(
      style = list(
        cell_fill(color = cor_cab),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels()
    ) |>
    opt_row_striping()
}

## --- 5.1 Premissas #### ---------------------------------------------------------
tab_premissas <- data.frame(
  Parametro = c("Data focal", "ARPU mensal (PPP)", "ARPU anual", "Horizonte",
                "Nº de acessos - safra 2024", "Nº de acessos - safra 2025",
                "Fator de adicionalidade", "Taxa 1 - DPMFi", "Taxa 2 - NTN-B"),
  Valor = c(as.character(data_focal), fmt_brl(arpu_mensal), fmt_brl(arpu_anual),
            paste(horizonte, "anos"),
            formatC(acessos_2024, format = "d", big.mark = "."),
            formatC(acessos_2025, format = "d", big.mark = "."),
            formatC(adicionalidade, format = "f", digits = 2, decimal.mark = ","),
            "11,04% a.a.", "6,28% a.a."),
  Fonte = c("Termo de Referência", "Anatel - Panorama PPPs 4T2024", "Cálculo",
            "European Commission (2014)", "QR FUST / Anatel", "QR FUST / Anatel",
            "A estimar (contrafactual)", "Tesouro - dez/2024", "Tesouro - dez/2024"),
  stringsAsFactors = FALSE
)
gt_premissas <- gt(tab_premissas) |>
  cols_label(Parametro = "Parâmetro", Valor = "Valor", Fonte = "Fonte") |>
  tab_header(title = "Premissas metodológicas - Acesso (ARPU, piso)") |>
  estilo()

print(gt_premissas)

## --- 5.2 Tabela do fluxo do benefício ano a ano #### ---
gt_fluxo_benef <- gt(fluxo_benef) |>
  cols_label(t = "t", Benef_safra_2024 = "Benefício 2024", Benef_safra_2025 = "Benefício 2025",
             Benef_total = "Benefício total",
             VP_2024_1104 = "VP 2024 (11,04%)", VP_2025_1104 = "VP 2025 (11,04%)",
             VP_2024_628 = "VP 2024 (6,28%)",  VP_2025_628 = "VP 2025 (6,28%)") |>
  fmt_currency(columns = c(Benef_safra_2024, Benef_safra_2025, Benef_total,
                           VP_2024_1104, VP_2025_1104, VP_2024_628, VP_2025_628),
               currency = "BRL", sep_mark = ".", dec_mark = ",") |>
  tab_header(title = "Fluxo do benefício - acesso (ano a ano)") |>
  estilo()

print(gt_fluxo_benef)

## --- 5.3 Resumo dos benefícios #### ---------------------------------------------
tab_rb <- data.frame(
  Safra       = c("2024", "2025", "TOTAL"),
  Acessos     = c(acessos_2024, acessos_2025, acessos_2024 + acessos_2025),
  Benef_anual = c(ben_anual_2024, ben_anual_2025, ben_anual_2024 + ben_anual_2025),
  VP_1104     = c(VPB_2024_1104, VPB_2025_1104, VPTB_1104),
  VP_628      = c(VPB_2024_628,  VPB_2025_628,  VPTB_628),
  stringsAsFactors = FALSE
)
gt_rb <- gt(tab_rb) |>
  cols_label(Acessos = "Nº de acessos", Benef_anual = "Benefício anual",
             VP_1104 = "VP (11,04%)", VP_628 = "VP (6,28%)") |>
  fmt_number(columns = Acessos, sep_mark = ".", decimals = 0) |>
  fmt_currency(columns = c(Benef_anual, VP_1104, VP_628),
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
  tab_header(title = "Fluxo mensal do custo — subsídio implícito (SAC)") |>
  estilo()

print(gt_fluxo_custo)

## --- 5.5 Resumo dos custos #### -------------------------------------------------
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
  fmt_percent(columns = c(TaxaFUST, Spread), decimals = 2,
              sep_mark = ".", dec_mark = ",") |>
  sub_missing(columns = c(TaxaFUST, Spread), missing_text = "–") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Safra == "TOTAL")) |>
  tab_header(title = "Resumo - Valor Presente dos Custos (subsídio implícito)") |>
  estilo()

print(gt_rc)

## --- 5.6 Resultado (VPL e RCB) #### ---------------------------------------------
tab_res <- data.frame(
  Indicador = c("VP dos benefícios (VPBT)", "VP dos custos (VPCT)",
                "VPL (benefício - custo)", "RCB (benefício / custo)"),
  Cenario_628  = c(fmt_brl(VPTB_628),  fmt_brl(VPTC_628),
                   fmt_brl(VPL_628),   formatC(RCB_628,  format = "f", digits = 2, decimal.mark = ",")),
  Cenario_1104 = c(fmt_brl(VPTB_1104), fmt_brl(VPTC_1104),
                   fmt_brl(VPL_1104),  formatC(RCB_1104, format = "f", digits = 2, decimal.mark = ",")),
  stringsAsFactors = FALSE
)
gt_res <- gt(tab_res) |>
  cols_label(Indicador = "", Cenario_628 = "Cenário 6,28% (NTN-B)",
             Cenario_1104 = "Cenário 11,04% (DPMFi)") |>
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(rows = Indicador %in%
                                     c("VPL (benefício - custo)", "RCB (benefício / custo)"))) |>
  tab_header(title = "Resultado da Análise Custo-Benefício - Acesso") |>
  tab_source_note(source_note = "Benefício = nº de acessos × ARPU anual (fluxo nivelado, 15 anos). Custo = subsídio implícito do crédito (SAC). Data focal: 2024.") |>
  estilo()

print(gt_res)

## ---------------------------------------------------------------------------
# 6) SALVAR ####
## ---------------------------------------------------------------------------

# Salvar cada tabela em HTML
dir.create("tabelas_acb", showWarnings = FALSE)
salvar <- function(g, nome) {
  gtsave(g, file.path("tabelas_acb", paste0(nome, ".html")))
  tryCatch(
    gtsave(g, file.path("tabelas_acb", paste0(nome, ".png"))),
    error = function(e) message("PNG de '", nome,
                                "' requer os pacotes webshot2/chromote + Chrome; HTML salvo.")
  )
}
salvar(gt_premissas, "01_premissas")
salvar(gt_fluxo_benef,       "02_beneficio")
salvar(gt_rb,        "03_resumo_beneficios")
salvar(gt_fluxo_custo,       "04_custo")
salvar(gt_rc,        "05_resumo_custos")
salvar(gt_res,       "06_resultado")

message("Tabelas geradas em: ", normalizePath("tabelas_acb"))
# ============================================================================
# FIM
# ============================================================================
