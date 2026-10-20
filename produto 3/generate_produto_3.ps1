$ErrorActionPreference = 'Stop'

$ScriptDir = Join-Path $PSScriptRoot 'code'
$OutputDir = Join-Path $PSScriptRoot 'analysis'
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

$impactScripts = @(
    '01 - Figuras principais.R',
    '02 - Heterogeneidades.R',
    '09 - Mapa amostra RAIS.R',
    '05 - Robustez especificacoes de matching.R',
    '06 - Robustez estimadores.R',
    '04 - Testes de robustez.R',
    '08 - Acessos banda larga fixa.R'
)

$acbScripts = @(
    'acb_fibra.R',
    'acb_acessos.R'
)

function Invoke-RScript {
    param(
        [Parameter(Mandatory = $true)]
        [string]$ScriptName
    )

    $script = Join-Path $ScriptDir $ScriptName
    if (-not (Test-Path -LiteralPath $script)) {
        throw "Script de replicacao ausente: $ScriptName"
    }

    Write-Output "Gerando: $ScriptName"
    & $Rscript $script
    if ($LASTEXITCODE -ne 0) {
        throw "Falha ao executar $ScriptName."
    }
}

New-Item -ItemType Directory -Path $OutputDir -Force | Out-Null

foreach ($scriptName in $impactScripts) {
    Invoke-RScript -ScriptName $scriptName
}

# As ACBs escrevem em caminhos relativos. Executá-las a partir de analysis/
# mantém todos os resultados gerados dentro da pasta padrão do produto.
Push-Location -LiteralPath $OutputDir
try {
    foreach ($scriptName in $acbScripts) {
        Invoke-RScript -ScriptName $scriptName
    }
}
finally {
    Pop-Location
}

Write-Output 'Produto 3 gerado em produto 3\analysis.'
