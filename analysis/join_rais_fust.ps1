param(
    [string]$Anos = '2018:2024',
    [string]$Fust = '',
    [string]$Rais = 'D:\rais',
    [string]$Rais2 = 'D:\rais2',
    [string]$Out = '',
    [string]$PanelOut = '',
    [switch]$OnlyPanelXlsx
)

$ErrorActionPreference = 'Stop'

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

$RepoRoot = Split-Path -Parent $PSScriptRoot
$Script = Join-Path $PSScriptRoot 'scripts\join_rais_fust.R'

if (-not $Fust) {
    $Fust = Join-Path $RepoRoot 'data\Base de Operações_FUST_compilada.xlsx'
}

if (-not $Out) {
    $Out = Join-Path $RepoRoot 'analysis\outputs\rais_fust'
}

if (-not $PanelOut) {
    $AnoLabel = $Anos -replace ':', '_' -replace ',', '_'
    $PanelOut = Join-Path $RepoRoot "data\Painel_CNPJ_FUST_RAIS_$AnoLabel.xlsx"
}

$RArgs = @(
    "--anos=$Anos",
    "--fust=$Fust",
    "--rais=$Rais",
    "--rais2=$Rais2",
    "--out=$Out",
    "--panel-out=$PanelOut"
)

if ($OnlyPanelXlsx) {
    $RArgs += '--only-panel-xlsx=true'
}

& $Rscript $Script $RArgs

if ($LASTEXITCODE -ne 0) {
    throw 'Falha ao executar o cruzamento RAIS/FUST.'
}
