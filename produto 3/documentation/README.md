# DiD escalonado mensal e trimestral — pós-Covid

Esta pasta estende a análise anual de `produto 3/analysis/psm_did` para frequências
mensal e trimestral.

## Desenho

- Janela: janeiro de 2022 a dezembro de 2024.
- Desfechos: `log(1 + estoque de empregados)` e
  `log(1 + massa salarial nominal mensal)`.
- Amostra: unidades pareadas pela análise PSM anual (`../analysis/psm_did/unidades_pareadas.csv`).
- Tratamento: mês (ou trimestre) da primeira contratação FUST observada na base
  compilada de operações.
- Callaway–Sant'Anna: `att_gt`, com grupo de controle *not-yet-treated*,
  antecipação zero e baseline universal no período relativo −1.
- SDID: estimado separadamente por coorte de adoção, com controles pareados e
  agregação ponderada pelo número de tratados.
- Trimestral: média dos três estoques/massas mensais do trimestre.
- Massa salarial: soma, por CNPJ raiz, das remunerações mensais nominais dos
  vínculos da RAIS. Janeiro–novembro usam `Vl Rem [mês] SC` e dezembro usa
  `Vl Remun Dezembro Nom`.

## Execução

No diretório raiz do repositório:

```powershell
& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" `
  "produto 3/code/frequencias_pos_covid/build_massa_salarial_mensal.R"

& "C:\Program Files\R\R-4.4.2\bin\Rscript.exe" `
  "produto 3/code/frequencias_pos_covid/run_did_frequencias.R"
```

Parâmetros opcionais:

```text
--inicio=2022-01 --fim=2024-12 --bootstrap-reps=0 --sdid-reps=50 --seed=3854035
--matched=... --monthly=... --operations=... --out=...
```

Por padrão, Callaway–Sant'Anna usa os erros-padrão analíticos agrupados por
unidade (`--bootstrap-reps=0`). Defina um número positivo para ativar o
*multiplier bootstrap*; em frequência mensal ele pode demorar vários minutos.

Os resultados, gráficos, painel analítico e auditorias são gravados em
`produto 3/analysis/frequencias_pos_covid/`. O arquivo `resumo_resultados.csv`
reúne as estimativas agregadas.
