$ErrorActionPreference = 'Stop'

$ProdutoDir = Split-Path -Parent $PSScriptRoot
$RepoRoot = Split-Path -Parent $ProdutoDir
$Compiler = Join-Path $PSScriptRoot 'append_base.R'
$DataDir = Join-Path $RepoRoot 'data'
$InputPath = (
    Get-ChildItem -LiteralPath $DataDir -Filter 'Base de Opera*_FUST_com_num_OPE_subcr*.xlsx' |
        Select-Object -First 1
).FullName

if (-not $InputPath) {
    throw 'Arquivo Base de Operacoes FUST com numero de operacao e subcredito nao encontrado.'
}

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
    throw 'Rscript nao encontrado.'
}

& $Rscript $Compiler

if ($LASTEXITCODE -ne 0) {
    throw 'Falha ao compilar a base FUST.'
}
