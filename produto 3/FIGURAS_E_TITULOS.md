# Figuras, tabelas e códigos — Produto 3

Índice dos resultados apresentados no documento final. Os arquivos são gerados
localmente em `analysis/` e não são versionados.

| Código | Resultado no documento | Conteúdo |
|---|---|---|
| `code/acb_fibra.R` | Figuras 1 e 2; Tabelas 1, 2, 4, 5 e 6 | Premissas, custos, benefícios, VPL e RCB da extensão de fibra óptica |
| `code/acb_acessos.R` | Figura 3; Tabelas 1, 3, 4, 7 e 8 | Premissas, custos, benefícios, VPL e RCB dos acessos à banda larga |
| `code/09 - Mapa amostra RAIS.R` | Figura 4 | Distribuição espacial das empresas tratadas e controles |
| `code/01 - Figuras principais.R` | Figuras 5, 6 e 7 | Efeitos dinâmicos sobre emprego, massa salarial e salário médio |
| `code/02 - Heterogeneidades.R` | Figuras 8, 9 e 10 | Efeitos heterogêneos sobre emprego, massa salarial e salário médio |
| `code/08 - Acessos banda larga fixa.R` | Figura A1.1 | Efeitos dinâmicos sobre acessos à banda larga |
| `code/05 - Robustez especificacoes de matching.R` | Figuras A2.1, A2.2 e A2.3 | Especificações alternativas de pareamento |
| `code/06 - Robustez estimadores.R` | Figuras A2.4, A2.5 e A2.6 | Estimadores de impacto alternativos |
| `code/04 - Testes de robustez.R` | Figuras A2.7, A2.8 e A2.9 | Inferência alternativa, controles, Conley e placebo |

## Arquivos auxiliares

- `code/_common.R`: outcomes, rótulos e identidade visual compartilhados.
- `code/_extract_rais_matching_banda_larga.R`: prepara as covariáveis da RAIS
  requeridas pela análise de acessos.

## Convenções

- As análises da RAIS usam emprego, massa salarial e salário médio.
- A extensão da Anatel usa o estoque de acessos de banda larga fixa em dezembro.
- Matching e estimadores exibem ponto e intervalo de confiança, sem linha entre
  coeficientes.
- Cores, tipografia e grades seguem a identidade visual usada no Produto 2.
