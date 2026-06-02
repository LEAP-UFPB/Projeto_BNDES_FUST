$ErrorActionPreference = 'Stop'

$ScriptDir = Join-Path $PSScriptRoot 'scripts'
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

$scripts = Get-ChildItem -LiteralPath $ScriptDir -Filter '*.R' |
    Where-Object { $_.Name -ne '_common.R' } |
    Sort-Object Name

foreach ($script in $scripts) {
    Write-Output "Gerando: $($script.Name)"
    & $Rscript $script.FullName

    if ($LASTEXITCODE -ne 0) {
        throw "Falha ao executar $($script.Name)."
    }
}

Write-Output 'Outputs separados gerados em analysis\outputs.'
