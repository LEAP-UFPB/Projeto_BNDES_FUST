# Produto 3 — Efeitos do apoio FUST sobre empresas

Este diretório reúne os códigos e os resultados finais da avaliação dos efeitos
do FUST sobre três outcomes: emprego, massa salarial e salário médio, todos em
logaritmo.

## Amostra principal

A especificação principal usa matching 1:1 sem reposição, com 224 empresas
tratadas e 224 controles. O tratamento é definido pelo primeiro apoio FUST e os
controles são empresas que não receberam o FUST.

## Organização dos códigos

1. `code/01 - Figuras principais.R`: event studies de Callaway–Sant'Anna.
2. `code/02 - Heterogeneidades.R`: modalidade, porte BNDES, setor, região,
   idade, qualificação, número de filiais/unidades e número de subcréditos FUST.
3. `code/03 - Tabela de balanceamento.R`: único arquivo Excel, com os testes de
   diferença de médias e o balanceamento da amostra.
4. `code/04 - Testes de robustez.R`: inferência alternativa, controles
   pré-tratamento interagidos com ano, erros espaciais de Conley (200 km) e
   placebo temporal.
5. `code/05 - Robustez especificacoes de matching.R`: matching com reposição, entropy
   matching, painel balanceado e propensity score 1:1. A especificação CS-Base
   não é repetida nessa figura.
6. `code/06 - Robustez estimadores.R`: TWFE, Gardner/did2s, BJS/imputation e
   SDID.
7. `code/07 - HonestDiD.R`: análise de sensibilidade a violações de tendências
   paralelas.

O arquivo `code/_common.R` centraliza outcomes, nomes e identidade visual.

## Heterogeneidades adicionadas

- **Filiais/unidades:** uma unidade versus mais de uma unidade, usando a
  informação observada antes do primeiro tratamento.
- **Idade da firma:** até 14 anos versus maior que 14 anos, conforme a mediana
  de idade das empresas tratadas.
- **Porte BNDES:** micro e pequena, média e grande.
- **Subcréditos:** um subcrédito versus mais de um subcrédito.

As heterogeneidades por cor/raça e coorte foram removidas.

## Resultados finais

`analysis/` contém somente 19 entregáveis:

- 1 workbook `.xlsx` com o teste de diferença de médias;
- 3 event studies de Callaway–Sant'Anna;
- 3 figuras de heterogeneidade;
- 3 figuras de robustez geral;
- 3 figuras de robustez das especificações de matching;
- 3 figuras de robustez por estimadores;
- 3 figuras HonestDiD.

Cada família de figuras tem um arquivo separado para emprego, massa salarial e
salário médio. A identidade visual segue o padrão do Produto 2: fonte Arial,
fundo branco, azul e verde institucionais do BNDES e legenda na parte inferior.
As figuras não possuem título interno; a identificação do outcome fica no nome
do arquivo e pode ser apresentada na legenda do documento.
Nas figuras de event study de matching e estimadores são exibidos apenas pontos
e intervalos de confiança, sem linhas ligando os coeficientes.

As bases intermediárias ficam em `../data/` e usam o prefixo `produto3_`.
Consulte `FIGURAS_E_TITULOS.md` para o inventário exato dos resultados.

## Relatório em PDF

O relatório interpretativo está em `relatorio/relatorio_resultados_fust.pdf`,
com fonte reproduzível em `relatorio/relatorio_resultados_fust.tex`. A seção de
heterogeneidades documenta os cortes, as amostras efetivas e a auditoria do
pareamento 1:1 sem reposição.
