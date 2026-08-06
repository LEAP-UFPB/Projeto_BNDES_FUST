$ErrorActionPreference = 'Stop'

$ScriptDir = Join-Path $PSScriptRoot 'code'
$RscriptCandidates = @(
    'C:\Program Files\R\R-4.5.3\bin\x64\Rscript.exe',
    'C:\Program Files\R\R-4.5.3\bin\Rscript.exe',
    'C:\Program Files\R\R-4.4.2\bin\x64\Rscript.exe',
    'C:\Program Files\R\R-4.4.2\bin\Rscript.exe',
    'Rscript.exe'
)

$Rscript = $RscriptCandidates | Where-Object {
    if ($_ -eq 'Rscript.exe') {
        [bool](Get-Command $_ -ErrorAction SilentlyContinue)
    }
    else {
        Test-Path -LiteralPath $_
    }
} | Select-Object -First 1

if (-not $Rscript) {
    throw 'Rscript nao encontrado. Verifique a instalacao do R ou ajuste $RscriptCandidates.'
}

$replicationScripts = @(
    'fig_composicao_por_origem.R',
    'fig_modalidade_contratado_vs_desembolsado.R',
    'fig_top_setor_cnae_valor_contratado.R',
    'fig_composicao_por_porte.R',
    'fig_top_uf_valor_contratado.R',
    'map_valor_contratado_por_uf.R',
    'fig_juros_e_prazo_total.R',
    'xlsx_juros_direto_indireto.R',
    'quadro_resultados_analises.R',
    'gerar_quadros_teoria_mudanca.R',
    'finalizar_outputs.R'
)

foreach ($scriptName in $replicationScripts) {
    $script = Join-Path $ScriptDir $scriptName
    if (-not (Test-Path -LiteralPath $script)) {
        throw "Script de replicacao ausente: $scriptName"
    }

    Write-Output "Gerando: $scriptName"
    & $Rscript $script

    if ($LASTEXITCODE -ne 0) {
        throw "Falha ao executar $scriptName."
    }
}

$OutputDir = Join-Path $PSScriptRoot 'analysis'
$ArchiveDir = Join-Path $PSScriptRoot 'archive_local'
$BuildId = Get-Date -Format 'yyyyMMdd_HHmmss'
$FlatDir = Join-Path $PSScriptRoot ".analysis_flat_$BuildId"
New-Item -ItemType Directory -Path $FlatDir | Out-Null

$candidates = Get-ChildItem -LiteralPath $OutputDir -Recurse -File |
    Where-Object { $_.Extension -in @('.png', '.xlsx') } |
    Sort-Object LastWriteTime -Descending

$selected = foreach ($file in $candidates) {
    $relative = $file.FullName.Substring($OutputDir.Length + 1)
    $parts = $relative.Split([IO.Path]::DirectorySeparatorChar)
    $top = $parts[0]
    $name = $file.Name
    $keep = $false

    if ($parts.Count -eq 1) {
        $keep = $name -match '^(Figura|Tabela|TABELA|Quadro|Quadros)'
        if ($name -match '^Figura\s+(14|15|16|17)\s+-\s+Frequência') {
            $keep = $false
        }
        if ($name -match '^TABELA\s+(23|30)[.]') {
            $keep = $false
        }
    }
    elseif ($top -like '1. Desempenho Operacional*') {
        $keep = $name -match '^(Figura|Tabela)\s+[0-9]+'
    }
    elseif ($top -like '2.1 Apoio Direto*') {
        $keep = $name -match '^(Figura\s+[0-9]+|Quadro(s)?\s+[0-9]+)'
    }
    elseif ($top -like '2.2 Apoio Indireto*') {
        $keep = $name -match '^Quadros\s+4\s+e\s+5'
    }

    if ($keep) {
        $file
    }
}

$selected = $selected |
    Group-Object Name |
    ForEach-Object { $_.Group | Select-Object -First 1 }

if (@($selected).Count -eq 0) {
    throw 'Nenhum entregavel final foi localizado.'
}

foreach ($file in $selected) {
    $finalName = switch -Exact ($file.Name) {
        'Figura 18 - Quantidade de indicadores por eixo e tipo.png' {
            'Figura 14 - Quantidade de indicadores por eixo e tipo.png'
        }
        'Figura 19 - Cobertura dos indicadores de eficácia e efetividade.png' {
            'Figura 15 - Cobertura dos indicadores de eficácia e efetividade (%).png'
        }
        'Figura 20 - Previsto versus realizado financeiro - comparação por eixo.png' {
            'Figura 16 - Previsto versus Realizado financeiro - comparação por eixo.png'
        }
        'Figura 9 - Desempenho por porte dos clientes - valor desembolsado.png' {
            'Figura 9 - Desempenho por porte dos clientes - valor desembolsado por porte.png'
        }
        'Figura 10 - Distribuição regional e por UF - número de operações.png' {
            'Figura 10 - Distribuição regional e por Unidade da Federação - número de operações por UF.png'
        }
        'Figura 11 - Distribuição regional e por UF - valor contratado.png' {
            'Figura 11 - Distribuição regional e por Unidade da Federação - valor contratado por UF.png'
        }
        'Figura 12 - Distribuição regional e por UF - mapa do valor contratado.png' {
            'Figura 12 - Distribuição regional e por Unidade da Federação - mapa valor contratado por UF.png'
        }
        'Figura 13 - Distribuição regional e por UF - valor desembolsado.png' {
            'Figura 13 - Distribuição regional e por Unidade da Federação - valor desembolsado por UF.png'
        }
        'Tabela 1 - Distribuição por faixa de taxa de juros das operações do FUST BNDES, 2024-2025.xlsx' {
            'Tabela 1 - Distribuição por faixa de taxa de juros - Apoio Direto, 2024–2025.xlsx'
        }
        'Tabela 2 - Distribuição dos juros por apoio direto e indireto.xlsx' {
            'Tabela 2 - Distribuição por faixa de taxa de juros - Apoio Indireto, 2024–2025.xlsx'
        }
        default { $file.Name }
    }
    Copy-Item -LiteralPath $file.FullName -Destination (Join-Path $FlatDir $finalName)
}

New-Item -ItemType Directory -Path $ArchiveDir -Force | Out-Null
$ArchivedBuild = Join-Path $ArchiveDir "analysis_build_$BuildId"
Move-Item -LiteralPath $OutputDir -Destination $ArchivedBuild
Move-Item -LiteralPath $FlatDir -Destination $OutputDir

$IndexScript = Join-Path $ScriptDir 'gerar_indice_figuras.R'
Write-Output 'Gerando: gerar_indice_figuras.R'
& $Rscript $IndexScript
if ($LASTEXITCODE -ne 0) {
    throw 'Falha ao executar gerar_indice_figuras.R.'
}

Write-Output 'Produto 2 gerado em produto 2\analysis.'
