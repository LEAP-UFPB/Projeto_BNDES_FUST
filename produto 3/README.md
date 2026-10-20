# Replicação do Produto 3

Este diretório concentra os scripts e a documentação de replicação do Produto 3.
Somente código e documentação são versionados; bases e resultados permanecem
locais.

## Estrutura

- `code/`: código necessário para gerar as análises apresentadas no Produto 3.
- `analysis/`: figuras, tabelas e planilhas geradas localmente; ignorada pelo Git.
- `generate_produto_3.ps1`: executor dos scripts na ordem de dependência.
- `REPLICACAO.Rmd`: roteiro reproduzível e documentado das análises.
- `FIGURAS_E_TITULOS.md`: relação entre os resultados do documento e seus scripts.

## Geração

No PowerShell, a partir da raiz do repositório:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\produto 3\generate_produto_3.ps1"
```

O executor roda, nesta ordem:

1. resultados principais de emprego, massa salarial e salário médio;
2. heterogeneidades e mapas da amostra;
3. especificações alternativas de matching, estimadores e demais testes de
   robustez;
4. efeitos sobre acessos de banda larga fixa;
5. análises de custo-benefício da fibra óptica e dos acessos.

Os scripts de impacto escrevem em `analysis/`. As duas ACBs são executadas com
`analysis/` como diretório de trabalho e, portanto, geram suas tabelas em
`analysis/tabelas_fibra/` e `analysis/tabelas_acb/`.

Também é possível usar o R Markdown:

```r
rmarkdown::render(
  "produto 3/REPLICACAO.Rmd",
  params = list(executar = TRUE)
)
```

Por padrão, o documento é renderizado sem executar as análises. Defina
`executar = TRUE` somente depois de disponibilizar as bases locais.

## Códigos de replicação

- `01 - Figuras principais.R`
- `02 - Heterogeneidades.R`
- `04 - Testes de robustez.R`
- `05 - Robustez especificacoes de matching.R`
- `06 - Robustez estimadores.R`
- `08 - Acessos banda larga fixa.R`
- `09 - Mapa amostra RAIS.R`
- `acb_fibra.R`
- `acb_acessos.R`

Os arquivos `_common.R` e `_extract_rais_matching_banda_larga.R` são auxiliares
necessários à execução.

Consulte `FIGURAS_E_TITULOS.md` para localizar o script responsável por cada
resultado utilizado no documento final.
