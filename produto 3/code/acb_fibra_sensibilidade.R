# ============================================================================
# ACB FUST / BNDES — FIBRA ÓPTICA — ANÁLISE DE SENSIBILIDADE
#
#   1  CASO BASE ............ 1.01 a 1.06 (premissas, benefício, resumo benef,
#                                          custo, resumo custo, resultado)
#   2  FAIXA DE JUROS ....... 2.01 a 2.07 (muda o custo: regenera 04, 05, 06)
#        só dois cenários: todas as faixas (total) e desembolso total + juros médio
#   3  MÉTODO DE DEPRECIAÇÃO. 3.01 a 3.07 (muda o benefício: regenera 02, 03, 06)
#   4  TAXA DE DESCONTO ..... 4.01        (comparativo por taxa, inclui TSD)
#   5  ELASTICIDADE ......... 5.01 a 5.04 (passo a passo)
#   6  VALORES DE REVERSÃO .. 6.01 a 6.02 (passo a passo)
#   7  QUADRO-SÍNTESE GERAL . 7.01
#
# Benefício = CAPEX evitado (km × R$/km), distribuído no horizonte pelo método
# de depreciação (base = SDA). Custo = subsídio implícito do crédito (SAC),
# idêntico ao do acesso, base na faixa "até 3%". Data focal 2024.
# Autoria: LEAP/BNDES
# ============================================================================

library(gt)
fmt_brl <- function(x) paste0("R$ ", formatC(x, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
f2 <- function(x) formatC(x, format = "f", digits = 2, decimal.mark = ",")
cor_cab <- "#2F5496"
estilo <- function(g) g |>
  opt_table_font(font = "Arial") |>
  tab_style(style = list(cell_fill(color = cor_cab), cell_text(color = "white", weight = "bold")),
            locations = cells_column_labels()) |>
  opt_row_striping()
dir.create("sensibilidade_fibra", showWarnings = FALSE)
salvar <- function(g, nome) {
  gtsave(g, file.path("sensibilidade_fibra", paste0(nome, ".html")))
  tryCatch(gtsave(g, file.path("sensibilidade_fibra", paste0(nome, ".png"))),
           error = function(e) message("PNG de '", nome, "' requer webshot2/chromote; HTML salvo."))
}

## ---------------------------------------------------------------------------
# PREMISSAS (CASO BASE) ####
## ---------------------------------------------------------------------------
H <- 15; horizonte <- 15; carencia <- 15; amortiz <- 66
cu0 <- 61763.45                 # custo unitário (R$/km) — CG-FUST/Anatel
ext_2024 <- 902.01; ext_2025 <- 3799.45
tx_dpmfi <- 0.1104; tx_ntnb <- 0.0628; rg24 <- 0.0628; rg25 <- 0.0723
metodo_base <- "SDA (acelerada)"
tranches <- data.frame(
  Safra = c("2024","2024","2024","2025","2025"), Faixa = c("Até 3%","3% a 7%","7% a 10%","Até 3%","3% a 7%"),
  Desembolso = c(316.6e6, 25.1e6, 8.0e6, 298.3e6, 15.9e6), TaxaFUST = c(0.0201,0.0512,0.0845,0.0262,0.0456),
  rg = c(rg24,rg24,rg24,rg25,rg25), defas = c(0,0,0,1,1), stringsAsFactors = FALSE)
agg_safra <- function(s, rg, defas) { f <- tranches[tranches$Safra == s, ]
  data.frame(Safra = s, Faixa = "Total (ponderado)", Desembolso = sum(f$Desembolso),
             TaxaFUST = sum(f$Desembolso*f$TaxaFUST)/sum(f$Desembolso), rg = rg, defas = defas, stringsAsFactors = FALSE) }
tr_base <- tranches[c(1,4), ]   # base = só a faixa "até 3%" em cada safra

## ---------------------------------------------------------------------------
# FUNÇÕES DE CÁLCULO E DE TABELA ####
## ---------------------------------------------------------------------------
pesos_metodo <- function(metodo) { h <- 1:H
  if (metodo == "SDA (acelerada)")        return((H - h + 1) / (H*(H+1)/2))
  if (metodo == "Linear (quotas const.)") return(rep(1/H, H))
  if (metodo == "Saldo decrescente") { r <- 2/H; w <- numeric(H); rem <- 1
    for (i in 1:H) { w[i] <- if (i < H) rem*r else rem; rem <- rem - w[i] }; return(w/sum(w)) }
  stop("método inválido") }
# Custo (idêntico ao acesso)
fluxo_tr <- function(desemb, rg, txfust, defas) {
  sm <- (1 + (rg - txfust))^(1/12) - 1; imn <- (1+tx_ntnb)^(1/12)-1; imd <- (1+tx_dpmfi)^(1/12)-1
  n <- carencia + amortiz; amt <- c(rep(0,carencia), rep(desemb/amortiz, amortiz))
  sd <- numeric(n); sd[1] <- desemb; for (k in 2:n) sd[k] <- sd[k-1] - amt[k-1]; sub <- sm*sd
  data.frame(Mes = 1:n, Amortizacao = amt, Saldo_Devedor = sd, Spread_mensal = sm, Subsidio = sub,
             VP_1104 = sub/(1+imd)^(1:n)/(1+tx_dpmfi)^defas, VP_628 = sub/(1+imn)^(1:n)/(1+tx_ntnb)^defas)
}
vp_custo <- function(trdf, taxa) { im <- (1+taxa)^(1/12)-1
  sum(sapply(seq_len(nrow(trdf)), function(i) { r <- trdf[i,]; sm <- (1+(r$rg-r$TaxaFUST))^(1/12)-1
    n <- carencia+amortiz; amt <- c(rep(0,carencia), rep(r$Desembolso/amortiz, amortiz))
    sd <- numeric(n); sd[1] <- r$Desembolso; for (k in 2:n) sd[k] <- sd[k-1]-amt[k-1]
    sum(sm*sd/(1+im)^(1:n)/(1+taxa)^r$defas) })) }
# Benefício (VPBT) da fibra a qualquer taxa
vp_benef <- function(cu, e24, e25, metodo, taxa) {
  p <- pesos_metodo(metodo); vb24 <- e24*cu; vb25 <- e25*cu
  anos <- 2024:(2024+H); t <- anos-2024
  w24 <- sapply(anos, function(a) if (a>=2024 & a<=2038) p[a-2023] else 0)
  w25 <- sapply(anos, function(a) if (a>=2025 & a<=2039) p[a-2024] else 0)
  sum((vb24*w24 + vb25*w25)/(1+taxa)^t)
}

gt_02_beneficio <- function(metodo, arquivo, nota = NULL) {
  p <- pesos_metodo(metodo); vb24 <- ext_2024*cu0; vb25 <- ext_2025*cu0
  anos <- 2024:(2024+H); t <- anos-2024
  w24 <- sapply(anos, function(a) if (a>=2024 & a<=2038) p[a-2023] else 0)
  w25 <- sapply(anos, function(a) if (a>=2025 & a<=2039) p[a-2024] else 0)
  b24 <- vb24*w24; b25 <- vb25*w25
  d <- data.frame(Ano=anos, t=t, Peso24=w24, Peso25=w25, Benef_2024=b24, Benef_2025=b25, Benef_total=b24+b25,
    VP_1104=(b24+b25)/(1+tx_dpmfi)^t, VP_628=(b24+b25)/(1+tx_ntnb)^t)
  g <- gt(d) |> cols_label(Ano="Ano", t="t", Peso24="Peso 2024", Peso25="Peso 2025",
    Benef_2024="Benefício 2024", Benef_2025="Benefício 2025", Benef_total="Benefício total",
    VP_1104="VP (11,04%)", VP_628="VP (6,28%)") |>
    fmt_number(columns=c(Peso24,Peso25), decimals=4, dec_mark=",") |>
    fmt_currency(columns=c(Benef_2024,Benef_2025,Benef_total,VP_1104,VP_628), currency="BRL", sep_mark=".", dec_mark=",") |>
    tab_header(title="Fluxo do benefício - fibra (CAPEX evitado por depreciação)", subtitle=paste0("Método ", metodo))
  if (!is.null(nota)) g <- g |> tab_source_note(nota)
  g <- estilo(g); print(g); salvar(g, arquivo)
}
gt_03_resumo_benef <- function(metodo, arquivo) {
  B628 <- vp_benef(cu0, ext_2024, 0, metodo, tx_ntnb) + vp_benef(cu0, 0, ext_2025, metodo, tx_ntnb)
  # por safra:
  v24n <- vp_benef(cu0, ext_2024, 0, metodo, tx_ntnb); v25n <- vp_benef(cu0, 0, ext_2025, metodo, tx_ntnb)
  v24d <- vp_benef(cu0, ext_2024, 0, metodo, tx_dpmfi); v25d <- vp_benef(cu0, 0, ext_2025, metodo, tx_dpmfi)
  capex24 <- ext_2024*cu0; capex25 <- ext_2025*cu0
  d <- data.frame(Safra=c("2024","2025","TOTAL"), Ext=c(ext_2024,ext_2025,ext_2024+ext_2025),
    Capex=c(capex24,capex25,capex24+capex25), VP_1104=c(v24d,v25d,v24d+v25d), VP_628=c(v24n,v25n,v24n+v25n))
  g <- gt(d) |> cols_label(Ext="Extensão (km)", Capex="CAPEX evitado (nominal)", VP_1104="VP (11,04%)", VP_628="VP (6,28%)") |>
    fmt_number(columns=Ext, sep_mark=".", dec_mark=",", decimals=2) |>
    fmt_currency(columns=c(Capex,VP_1104,VP_628), currency="BRL", sep_mark=".", dec_mark=",") |>
    tab_style(style=cell_text(weight="bold"), locations=cells_body(rows=Safra=="TOTAL")) |>
    tab_header(title="Resumo - Valor Presente dos Benefícios", subtitle=paste0("Método ", metodo)) |> estilo()
  print(g); salvar(g, arquivo); c(VPBT_628=v24n+v25n, VPBT_1104=v24d+v25d)
}
gt_04_custo <- function(trdf, arquivo, nota = NULL) {
  multi <- any(table(trdf$Safra) > 1)
  fl <- do.call(rbind, lapply(seq_len(nrow(trdf)), function(i) { r <- trdf[i,]
    grp <- if (multi) paste0(r$Safra, " · ", r$Faixa) else r$Safra
    cbind(Grupo=grp, fluxo_tr(r$Desembolso, r$rg, r$TaxaFUST, r$defas)) }))
  g <- gt(fl, groupname_col="Grupo") |> cols_label(Mes="Mês", Amortizacao="Amortização", Saldo_Devedor="Saldo devedor",
    Spread_mensal="Spread (a.m.)", Subsidio="Subsídio mensal", VP_1104="VP (11,04%)", VP_628="VP (6,28%)") |>
    fmt_currency(columns=c(Amortizacao,Saldo_Devedor,Subsidio,VP_1104,VP_628), currency="BRL", sep_mark=".", dec_mark=",") |>
    fmt_number(columns=Spread_mensal, decimals=6, dec_mark=",") |>
    tab_header(title="Fluxo mensal do custo - subsídio implícito (SAC)")
  if (!is.null(nota)) g <- g |> tab_source_note(nota)
  g <- estilo(g); print(g); salvar(g, arquivo)
}
gt_05_resumo_custos <- function(trdf, arquivo) {
  d <- do.call(rbind, lapply(seq_len(nrow(trdf)), function(i) { r <- trdf[i,]; fl <- fluxo_tr(r$Desembolso,r$rg,r$TaxaFUST,r$defas)
    data.frame(Safra=r$Safra, Faixa=r$Faixa, Desembolso=r$Desembolso, TaxaFUST=r$TaxaFUST, Spread=r$rg-r$TaxaFUST,
      Subsidio_nom=sum(fl$Subsidio), VP_1104=sum(fl$VP_1104), VP_628=sum(fl$VP_628), stringsAsFactors=FALSE) }))
  tot <- data.frame(Safra="TOTAL", Faixa="VPCT", Desembolso=sum(d$Desembolso), TaxaFUST=NA, Spread=NA,
    Subsidio_nom=sum(d$Subsidio_nom), VP_1104=sum(d$VP_1104), VP_628=sum(d$VP_628))
  g <- gt(rbind(d,tot)) |> cols_label(Faixa="Faixa", Desembolso="Valor desembolsado", TaxaFUST="Taxa FUST (média)",
    Spread="Spread anual", Subsidio_nom="Subsídio nominal", VP_1104="VP (11,04%)", VP_628="VP (6,28%)") |>
    fmt_currency(columns=c(Desembolso,Subsidio_nom,VP_1104,VP_628), currency="BRL", sep_mark=".", dec_mark=",") |>
    fmt_percent(columns=c(TaxaFUST,Spread), decimals=2, sep_mark=".", dec_mark=",") |>
    sub_missing(columns=c(TaxaFUST,Spread), missing_text="–") |>
    tab_style(style=cell_text(weight="bold"), locations=cells_body(rows=Safra=="TOTAL")) |>
    tab_header(title="Resumo - Valor Presente dos Custos (subsídio implícito)") |> estilo()
  print(g); salvar(g, arquivo); c(VPCT_628=sum(d$VP_628), VPCT_1104=sum(d$VP_1104))
}
gt_06_resultado <- function(vpbt628, vpct628, vpbt1104, vpct1104, arquivo, subt = NULL) {
  d <- data.frame(Indicador=c("VP dos benefícios (VPBT)","VP dos custos (VPCT)","VPL (benefício - custo)","RCB (benefício / custo)"),
    C628=c(fmt_brl(vpbt628),fmt_brl(vpct628),fmt_brl(vpbt628-vpct628),f2(vpbt628/vpct628)),
    C1104=c(fmt_brl(vpbt1104),fmt_brl(vpct1104),fmt_brl(vpbt1104-vpct1104),f2(vpbt1104/vpct1104)), stringsAsFactors=FALSE)
  g <- gt(d) |> cols_label(Indicador="", C628="Cenário 6,28% (NTN-B)", C1104="Cenário 11,04% (DPMFi)") |>
    tab_style(style=cell_text(weight="bold"), locations=cells_body(rows=Indicador %in% c("VPL (benefício - custo)","RCB (benefício / custo)"))) |>
    tab_header(title="Resultado da Análise Custo-Benefício - Fibra", subtitle=subt) |>
    tab_source_note("Benefício = CAPEX evitado (km × R$/km) distribuído por depreciação. Custo = subsídio implícito do crédito (SAC). Data focal 2024.") |> estilo()
  print(g); salvar(g, arquivo)
}
# Resumo consolidado de um bloco (vários cenários lado a lado, duas taxas)
gt_resumo_bloco <- function(df, titulo, arquivo, subt = NULL, col1 = "Cenário") {
  df$VPL628  <- df$VPBT628  - df$VPCT628;  df$RCB628  <- df$VPBT628  / df$VPCT628
  df$VPL1104 <- df$VPBT1104 - df$VPCT1104; df$RCB1104 <- df$VPBT1104 / df$VPCT1104
  df <- df[, c("Cenario","VPBT628","VPCT628","VPL628","RCB628","VPBT1104","VPCT1104","VPL1104","RCB1104")]
  g <- gt(df) |>
    cols_label(Cenario=col1, VPBT628="VPBT", VPCT628="VPCT", VPL628="VPL", RCB628="RCB",
               VPBT1104="VPBT", VPCT1104="VPCT", VPL1104="VPL", RCB1104="RCB") |>
    tab_spanner(label="Taxa 6,28% (NTN-B)",  columns=c(VPBT628,VPCT628,VPL628,RCB628)) |>
    tab_spanner(label="Taxa 11,04% (DPMFi)", columns=c(VPBT1104,VPCT1104,VPL1104,RCB1104)) |>
    fmt_currency(columns=c(VPBT628,VPCT628,VPL628,VPBT1104,VPCT1104,VPL1104), currency="BRL", sep_mark=".", dec_mark=",") |>
    fmt_number(columns=c(RCB628,RCB1104), decimals=2, dec_mark=",") |>
    tab_style(style=cell_text(weight="bold"), locations=cells_body(rows=1)) |>
    tab_header(title=titulo, subtitle=subt) |> estilo()
  print(g); salvar(g, arquivo)
}

## ===========================================================================
# 1 · CASO BASE (1.01 a 1.06) ####
## ===========================================================================
tab01 <- data.frame(
  Parametro = c("Data focal","Custo unitário (CG-FUST)","Extensão - safra 2024","Extensão - safra 2025",
                "CAPEX evitado (total)","Método de depreciação (base)","Horizonte","Taxa 1 - DPMFi","Taxa 2 - NTN-B"),
  Valor = c("2024", paste0(fmt_brl(cu0), "/km"), "902,01 km", "3.799,45 km",
            fmt_brl((ext_2024+ext_2025)*cu0), "SDA (soma dos dígitos dos anos)", "15 anos", "11,04% a.a.", "6,28% a.a."),
  Fonte = c("Termo de Referência","CG-FUST / Anatel","QR FUST / Anatel","QR FUST / Anatel",
            "Cálculo","MCASP / literatura","European Commission (2014)","Tesouro - dez/2024","Tesouro - dez/2024"),
  stringsAsFactors = FALSE)
g01 <- gt(tab01) |> cols_label(Parametro="Parâmetro", Valor="Valor", Fonte="Fonte") |>
  tab_header(title="Premissas metodológicas - Fibra (CAPEX evitado)") |> estilo()
print(g01); salvar(g01, "1.01_premissas")
gt_02_beneficio(metodo_base, "1.02_beneficio")
VPBT_base <- gt_03_resumo_benef(metodo_base, "1.03_resumo_beneficios")
gt_04_custo(tr_base, "1.04_custo")
VPCT_base <- gt_05_resumo_custos(tr_base, "1.05_resumo_custos")
gt_06_resultado(VPBT_base["VPBT_628"], VPCT_base["VPCT_628"], VPBT_base["VPBT_1104"], VPCT_base["VPCT_1104"], "1.06_resultado")

## ===========================================================================
# 2 · FAIXA DE JUROS (2.01 a 2.07) — regenera 04, 05, 06 ####
## ===========================================================================
cen_juros <- list(
  list(id="todas",tr=tranches,              rot="Todas as faixas (total real, faixa a faixa)",  nota="Inclui todas as faixas do apoio direto, parcela a parcela."),
  list(id="agreg",tr=rbind(agg_safra("2024",rg24,0),agg_safra("2025",rg25,1)), rot="Desembolso total + juros médio", nota="Desembolso total por safra com a taxa FUST média ponderada."))
res_juros <- data.frame(Cenario="Caso base", VPBT628=VPBT_base["VPBT_628"], VPCT628=VPCT_base["VPCT_628"],
                        VPBT1104=VPBT_base["VPBT_1104"], VPCT1104=VPCT_base["VPCT_1104"], row.names=NULL)
k <- 1
for (cj in cen_juros) {
  n04 <- sprintf("2.%02d_juros_%s_custo", k, cj$id)
  n05 <- sprintf("2.%02d_juros_%s_resumo_custos", k+1, cj$id)
  n06 <- sprintf("2.%02d_juros_%s_resultado", k+2, cj$id)
  gt_04_custo(cj$tr, n04, nota=cj$nota)
  v <- gt_05_resumo_custos(cj$tr, n05)
  gt_06_resultado(VPBT_base["VPBT_628"], v["VPCT_628"], VPBT_base["VPBT_1104"], v["VPCT_1104"], n06,
                  subt=paste0("Cenário de custo: ", cj$rot, " (benefício fixo no base)"))
  res_juros <- rbind(res_juros, data.frame(Cenario=cj$rot, VPBT628=VPBT_base["VPBT_628"], VPCT628=v["VPCT_628"],
                        VPBT1104=VPBT_base["VPBT_1104"], VPCT1104=v["VPCT_1104"], row.names=NULL))
  k <- k + 3
}
gt_resumo_bloco(res_juros, "2.07 · Resumo - sensibilidade à faixa de juros", "2.07_juros_resumo",
                subt="Benefício (VPBT) fixo no caso base; muda apenas o custo", col1="Cenário de custo")

## ===========================================================================
# 3 · MÉTODO DE DEPRECIAÇÃO (3.01 a 3.07) — regenera 02, 03, 06 ####
## ===========================================================================
cen_metodo <- list(list(id="linear", m="Linear (quotas const.)"), list(id="saldo", m="Saldo decrescente"))
res_metodo <- data.frame(Cenario="SDA (base)", VPBT628=VPBT_base["VPBT_628"], VPCT628=VPCT_base["VPCT_628"],
                         VPBT1104=VPBT_base["VPBT_1104"], VPCT1104=VPCT_base["VPCT_1104"], row.names=NULL)
k <- 1
for (cm in cen_metodo) {
  nota <- paste0("Depreciação ", cm$m, ": o mesmo CAPEX evitado é distribuído no horizonte por esse método; o custo não muda.")
  n02 <- sprintf("3.%02d_metodo_%s_beneficio", k, cm$id)
  n03 <- sprintf("3.%02d_metodo_%s_resumo_beneficios", k+1, cm$id)
  n06 <- sprintf("3.%02d_metodo_%s_resultado", k+2, cm$id)
  gt_02_beneficio(cm$m, n02, nota=nota)
  v <- gt_03_resumo_benef(cm$m, n03)
  gt_06_resultado(v["VPBT_628"], VPCT_base["VPCT_628"], v["VPBT_1104"], VPCT_base["VPCT_1104"], n06,
                  subt=paste0("Depreciação ", cm$m, " (custo fixo no base)"))
  res_metodo <- rbind(res_metodo, data.frame(Cenario=cm$m, VPBT628=v["VPBT_628"], VPCT628=VPCT_base["VPCT_628"],
                        VPBT1104=v["VPBT_1104"], VPCT1104=VPCT_base["VPCT_1104"], row.names=NULL))
  k <- k + 3
}
gt_resumo_bloco(res_metodo, "3.07 · Resumo - sensibilidade ao método de depreciação", "3.07_metodo_resumo",
                subt="Custo (VPCT) fixo no caso base; muda apenas a distribuição temporal do benefício", col1="Método de depreciação")

## ===========================================================================
# 4 · TAXA DE DESCONTO (4.01) — comparativo por taxa, inclui a TSD ####
## ===========================================================================
taxas <- c(0.0571, 0.0628, 0.085, 0.1104, 0.1144)
rotx  <- c("5,71% (TSD mín., Brasil)","6,28% (NTN-B)","8,50% (TSD central, Brasil)","11,04% (DPMFi)","11,44% (TSD máx., Brasil)")
tab4 <- do.call(rbind, Map(function(tx, rr) {
  B <- vp_benef(cu0, ext_2024, ext_2025, metodo_base, tx); C <- vp_custo(tr_base, tx)
  data.frame(Taxa=rr, VPBT=B, VPCT=C, VPL=B-C, RCB=B/C) }, taxas, rotx))
g4 <- gt(tab4) |> cols_label(Taxa="Taxa de desconto", VPBT="VPBT", VPCT="VPCT", VPL="VPL", RCB="RCB") |>
  fmt_currency(columns=c(VPBT,VPCT,VPL), currency="BRL", sep_mark=".", dec_mark=",") |>
  fmt_number(columns=RCB, decimals=2, dec_mark=",") |>
  tab_header(title="Sensibilidade à taxa de desconto - Fibra", subtitle="Inclui a Taxa Social de Desconto do Brasil (8,5% central; faixa 5,71%-11,44%)") |>
  tab_source_note("TSD: Nota Técnica SEI nº 19911/2020/ME (SDI/Ministério da Economia).") |> estilo()
print(g4); salvar(g4, "4.01_taxa_desconto")

## ===========================================================================
# 5 · ELASTICIDADE (5.01 a 5.04) — passo a passo ####
## ===========================================================================
VPL_b628  <- vp_benef(cu0,ext_2024,ext_2025,metodo_base,tx_ntnb)  - vp_custo(tr_base,tx_ntnb)
VPL_b1104 <- vp_benef(cu0,ext_2024,ext_2025,metodo_base,tx_dpmfi) - vp_custo(tr_base,tx_dpmfi)
# 5.01 ponto de partida
g501 <- gt(data.frame(Indicador=c("VPBT","VPCT","VPL"),
    C628=c(fmt_brl(VPBT_base["VPBT_628"]),fmt_brl(VPCT_base["VPCT_628"]),fmt_brl(VPL_b628)),
    C1104=c(fmt_brl(VPBT_base["VPBT_1104"]),fmt_brl(VPCT_base["VPCT_1104"]),fmt_brl(VPL_b1104)))) |>
  cols_label(Indicador="", C628="6,28%", C1104="11,04%") |>
  tab_header(title="5.1 · Ponto de partida (VPL base de referência)") |> estilo()
print(g501); salvar(g501, "5.01_elast_vpl_base")
# 5.02 perturbação de cada variável (+1%)
g502 <- gt(data.frame(
  Variavel=c("Custo unitário (R$/km)","Extensão (total km)","Desembolso (base)","Taxa FUST (2024/2025)"),
  Base=c("R$ 61.763,45","4.701,46 km","R$ 614,9 mi","2,01% / 2,62%"),
  Mais1=c("R$ 62.381,08","4.748,47 km","R$ 621,0 mi","2,03% / 2,65%"))) |>
  cols_label(Variavel="Variável", Base="Valor base", Mais1="Valor com +1%") |>
  tab_header(title="5.2 · Perturbação de cada variável em +1%") |> estilo()
print(g502); salvar(g502, "5.02_elast_perturbacao")
# 5.03 VPL recalculado e elasticidade
el <- function(VPLp, VPLb) (VPLp/VPLb - 1)/0.01
tr_d <- tr_base; tr_d$Desembolso <- tr_d$Desembolso*1.01
tr_t <- tr_base; tr_t$TaxaFUST <- tr_t$TaxaFUST*1.01
vplf <- function(cu,e24,e25,trd,taxa) vp_benef(cu,e24,e25,metodo_base,taxa) - vp_custo(trd,taxa)
lin <- function(nome, p628, p1104) data.frame(Variavel=nome, VPLbase=VPL_b628, VPLmais1=p628,
  Elast628=el(p628,VPL_b628), Elast1104=el(p1104,VPL_b1104))
tab503 <- rbind(
  lin("Custo unitário", vplf(cu0*1.01,ext_2024,ext_2025,tr_base,tx_ntnb),   vplf(cu0*1.01,ext_2024,ext_2025,tr_base,tx_dpmfi)),
  lin("Extensão",       vplf(cu0,ext_2024*1.01,ext_2025*1.01,tr_base,tx_ntnb), vplf(cu0,ext_2024*1.01,ext_2025*1.01,tr_base,tx_dpmfi)),
  lin("Desembolso",     vplf(cu0,ext_2024,ext_2025,tr_d,tx_ntnb),           vplf(cu0,ext_2024,ext_2025,tr_d,tx_dpmfi)),
  lin("Taxa FUST",      vplf(cu0,ext_2024,ext_2025,tr_t,tx_ntnb),           vplf(cu0,ext_2024,ext_2025,tr_t,tx_dpmfi)))
g503 <- gt(tab503) |> cols_label(Variavel="Variável", VPLbase="VPL base (6,28%)", VPLmais1="VPL com +1% (6,28%)",
    Elast628="Elasticidade (6,28%)", Elast1104="Elasticidade (11,04%)") |>
  fmt_currency(columns=c(VPLbase,VPLmais1), currency="BRL", sep_mark=".", dec_mark=",") |>
  fmt_number(columns=c(Elast628,Elast1104), decimals=2, dec_mark=",") |>
  tab_header(title="5.3 · VPL recalculado com +1% e elasticidade") |> estilo()
print(g503); salvar(g503, "5.03_elast_calculo")
# 5.04 variáveis críticas
tab504 <- tab503[, c("Variavel","Elast628","Elast1104")]
tab504$Critica <- ifelse(pmax(abs(tab504$Elast628), abs(tab504$Elast1104)) > 1, "Sim", "Não")
g504 <- gt(tab504) |> cols_label(Variavel="Variável", Elast628="Elasticidade (6,28%)", Elast1104="Elasticidade (11,04%)", Critica="Crítica?") |>
  fmt_number(columns=c(Elast628,Elast1104), decimals=2, dec_mark=",") |>
  tab_header(title="5.4 · Variáveis críticas", subtitle="Crítica quando |elasticidade| > 1 (EUROPEAN COMMISSION, 2014, seção 2.9.1, p. 67)") |> estilo()
print(g504); salvar(g504, "5.04_elast_criticas")

## ===========================================================================
# 6 · VALORES DE REVERSÃO (6.01 a 6.02) — passo a passo ####
## ===========================================================================
RCB628 <- VPBT_base["VPBT_628"]/VPCT_base["VPCT_628"]; RCB1104 <- VPBT_base["VPBT_1104"]/VPCT_base["VPCT_1104"]
# 6.01 indicadores base
g601 <- gt(data.frame(Indicador=c("VPBT","VPCT","VPL","RCB"),
    C628=c(fmt_brl(VPBT_base["VPBT_628"]),fmt_brl(VPCT_base["VPCT_628"]),fmt_brl(VPL_b628),f2(RCB628)),
    C1104=c(fmt_brl(VPBT_base["VPBT_1104"]),fmt_brl(VPCT_base["VPCT_1104"]),fmt_brl(VPL_b1104),f2(RCB1104)))) |>
  cols_label(Indicador="", C628="6,28%", C1104="11,04%") |>
  tab_header(title="6.1 · Ponto de partida (indicadores do caso base)") |> estilo()
print(g601); salvar(g601, "6.01_reversao_base")
# 6.02 derivação dos limites
tab602 <- data.frame(
  Indicador=c("Custo unitário de equilíbrio (R$/km)","Extensão de equilíbrio (% do base)","Redução máxima do benefício","Aumento máximo do custo"),
  Formula=c("cu / RCB","1 / RCB","1 − 1/RCB","RCB − 1"),
  V628=c(fmt_brl(cu0/RCB628), formatC(100/RCB628,format="f",digits=1,decimal.mark=",") |> paste0("%"),
         formatC(100*(1-1/RCB628),format="f",digits=1,decimal.mark=",") |> paste0("%"),
         formatC(100*(RCB628-1),format="f",digits=1,decimal.mark=",") |> paste0("%")),
  V1104=c(fmt_brl(cu0/RCB1104), formatC(100/RCB1104,format="f",digits=1,decimal.mark=",") |> paste0("%"),
          formatC(100*(1-1/RCB1104),format="f",digits=1,decimal.mark=",") |> paste0("%"),
          formatC(100*(RCB1104-1),format="f",digits=1,decimal.mark=",") |> paste0("%")), stringsAsFactors=FALSE)
g602 <- gt(tab602) |> cols_label(Indicador="Indicador (zera o VPL)", Formula="Fórmula", V628="6,28%", V1104="11,04%") |>
  tab_header(title="6.2 · Derivação dos valores de reversão", subtitle="Limites em que o VPL chega a zero (EUROPEAN COMMISSION, 2014, p. 68)") |> estilo()
print(g602); salvar(g602, "6.02_reversao_derivacao")

## ===========================================================================
# 7 · QUADRO-SÍNTESE GERAL (7.01) — elasticidade + criticidade + reversão ####
## ===========================================================================
sint <- tab504  # Variavel, Elast628, Elast1104, Critica
sint$Lado <- c("Benefício","Benefício","Custo","Custo")
sint$Reversao <- c(
  sprintf("Custo unitário cairia para R$ %s (%.1f%% do base)", f2(cu0/RCB628), 100/RCB628),
  sprintf("Extensão cairia para %.1f%% do base", 100/RCB628),
  sprintf("Custo subiria +%.1f%% sobre o base", 100*(RCB628-1)),
  "Taxa FUST teria de subir muito além do teto de 10%")
sint <- rbind(sint, data.frame(Variavel="Taxa de desconto", Elast628=NA, Elast1104=NA, Critica="Não",
  Lado="Ambos", Reversao="VPL positivo em todo o intervalo plausível (5,71% a 11,44%)"))
sint <- sint[, c("Variavel","Lado","Elast628","Elast1104","Critica","Reversao")]
g701 <- gt(sint) |>
  cols_label(Variavel="Variável", Lado="Lado", Elast628="Elasticidade (6,28%)",
             Elast1104="Elasticidade (11,04%)", Critica="Crítica?", Reversao="Valor de reversão (zera o VPL)") |>
  fmt_number(columns=c(Elast628,Elast1104), decimals=2, dec_mark=",") |>
  sub_missing(columns=c(Elast628,Elast1104), missing_text="–") |>
  tab_style(style=cell_text(weight="bold"), locations=cells_body(rows=Critica=="Sim")) |>
  tab_header(title="7.1 · Quadro-síntese da análise de sensibilidade - Fibra",
             subtitle="Elasticidade, criticidade (|elast.| > 1) e valor de reversão por variável") |>
  tab_source_note("Valores de reversão derivados na tabela 6.02. Critério de criticidade em EUROPEAN COMMISSION (2014, seção 2.9.1, p. 67).") |>
  estilo()
print(g701); salvar(g701, "7.01_sintese_geral")

message("Tabelas salvas em: ", normalizePath("sensibilidade_fibra"))
# ============================================================================
# FIM
# ============================================================================
