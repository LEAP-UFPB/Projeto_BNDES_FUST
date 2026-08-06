# Produto 3 — Avaliação de impacto

O Produto 3 reúne as análises de PSM-DID e as extensões em frequência mensal.

## Estrutura

- `code/psm_did/`: estimação anual, diagnósticos e estimadores alternativos.
- `code/frequencias_pos_covid/`: construção da massa salarial mensal e DID mensal.
- `analysis/psm_did/`: resultados da análise anual.
- `analysis/frequencias_pos_covid/`: resultados das análises mensais.
- `documentation/`: metodologia, relatório e instruções.
- `FIGURAS_E_TITULOS.md`: ligação entre cada figura, seu título e o código gerador.

## Códigos principais

- `code/psm_did/run_psm_did_fust.R`
- `code/psm_did/plot_alternative_estimators.R`
- `code/psm_did/run_synthdid_event_study.R`
- `code/psm_did/optimize_pretrend_without_outcomes.R`
- `code/frequencias_pos_covid/build_massa_salarial_mensal.R`
- `code/frequencias_pos_covid/run_did_frequencias.R`
