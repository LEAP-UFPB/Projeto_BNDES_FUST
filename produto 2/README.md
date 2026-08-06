# Replicação do Produto 2

Este diretório concentra os scripts e os resultados do Produto 2.

## Estrutura

- `code/`: código necessário para gerar as análises.
- `analysis/`: pasta única com somente as figuras PNG e tabelas XLSX usadas no documento.
- `archive_local/`: cópias antigas e backups locais; ignorado pelo Git.

## Geração

No PowerShell, a partir da raiz do repositório:

```powershell
powershell.exe -NoProfile -ExecutionPolicy Bypass -File ".\produto 2\generate_produto_2.ps1"
```

O executor roda, nesta ordem:

1. scripts das Figuras 1–13, sobre desempenho operacional;
2. `fig_juros_e_prazo_total.R`;
3. `xlsx_juros_direto_indireto.R`;
4. `quadro_resultados_analises.R`;
5. `gerar_quadros_teoria_mudanca.R` — gera os Quadros 2 a 5, para apoio direto e indireto;
6. `finalizar_outputs.R` — garante transparência real nos PNGs;
7. `gerar_indice_figuras.R` — relaciona código, figura, título e arquivo.

Ao final, o executor achata os resultados em `analysis/`, remove arquivos auxiliares
e mantém somente entregáveis numerados com os títulos do documento.

Consulte `FIGURAS_E_TITULOS.md` para localizar o código responsável por cada figura.

As bases de entrada ficam em `data/`. Os resultados finais são escritos diretamente
em `analysis/`, sem subpastas.

Os scripts de preparação das bases permanecem separados do executor principal:

- `code/compile_fust_base.ps1`
- `code/append_base.R`
