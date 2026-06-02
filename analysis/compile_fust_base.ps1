$ErrorActionPreference = 'Stop'

Add-Type -AssemblyName System.IO.Compression.FileSystem
Add-Type -AssemblyName System.IO.Compression

$DataDir = Join-Path $PSScriptRoot '..\data'
$InputPath = (Get-ChildItem -LiteralPath $DataDir -Filter 'Base de Opera*_FUST (enviada) (1).xlsx' | Select-Object -First 1).FullName
if (-not $InputPath) {
    throw 'Arquivo de entrada nao encontrado na pasta data.'
}
$CsvPath = Join-Path $PSScriptRoot '..\data\Base_de_Operacoes_FUST_compilada.csv'
$XlsxPath = Join-Path $PSScriptRoot '..\data\Base_de_Operacoes_FUST_compilada.xlsx'

function Remove-Accents([string]$Text) {
    if ($null -eq $Text) { return '' }
    $normalized = $Text.Normalize([Text.NormalizationForm]::FormD)
    $builder = New-Object Text.StringBuilder
    foreach ($ch in $normalized.ToCharArray()) {
        if ([Globalization.CharUnicodeInfo]::GetUnicodeCategory($ch) -ne [Globalization.UnicodeCategory]::NonSpacingMark) {
            [void]$builder.Append($ch)
        }
    }
    $builder.ToString().Normalize([Text.NormalizationForm]::FormC)
}

function Normalize-Key([string]$Text) {
    $value = (Remove-Accents $Text).ToLowerInvariant()
    $value = $value -replace '\(\*\)', ''
    $value = $value -replace '\s+', ' '
    $value.Trim()
}

function Read-ZipEntryText($Zip, [string]$Name) {
    $entry = $Zip.GetEntry($Name)
    if ($null -eq $entry) { return $null }
    $reader = New-Object IO.StreamReader($entry.Open())
    try { $reader.ReadToEnd() } finally { $reader.Close() }
}

function Read-ZipEntryXml($Zip, [string]$Name) {
    $text = Read-ZipEntryText $Zip $Name
    if ($null -eq $text) { return $null }
    [xml]$text
}

function Get-ColIndex([string]$CellRef) {
    $letters = ([regex]::Match($CellRef, '^[A-Z]+')).Value
    $index = 0
    foreach ($ch in $letters.ToCharArray()) {
        $index = ($index * 26) + ([int][char]$ch - [int][char]'A' + 1)
    }
    $index
}

function Get-CellValue($Cell, [string[]]$SharedStrings) {
    if ($null -eq $Cell) { return '' }
    switch ($Cell.t) {
        's' { return $SharedStrings[[int]$Cell.v] }
        'inlineStr' { return (($Cell.is.t | ForEach-Object { $_.'#text' }) -join '') }
        'str' { return [string]$Cell.v }
        default { return [string]$Cell.v }
    }
}

function Get-RowsFromSheet($Zip, [string]$SheetPath, [string[]]$SharedStrings) {
    $sheetXml = Read-ZipEntryXml $Zip ('xl/' + $SheetPath)
    $ns = New-Object Xml.XmlNamespaceManager($sheetXml.NameTable)
    $ns.AddNamespace('m', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')

    foreach ($row in $sheetXml.SelectNodes('//m:sheetData/m:row', $ns)) {
        $values = @{}
        $maxColumn = 0
        foreach ($cell in $row.SelectNodes('m:c', $ns)) {
            $columnIndex = Get-ColIndex $cell.r
            $values[$columnIndex] = Get-CellValue $cell $SharedStrings
            if ($columnIndex -gt $maxColumn) { $maxColumn = $columnIndex }
        }
        $rowValues = for ($i = 1; $i -le $maxColumn; $i++) {
            if ($values.ContainsKey($i)) { $values[$i] } else { '' }
        }
        [pscustomobject]@{
            RowNumber = [int]$row.r
            Values = [string[]]$rowValues
        }
    }
}

function Convert-ExcelDate([string]$Value) {
    $numeric = 0.0
    if ([double]::TryParse($Value, [Globalization.NumberStyles]::Float, [Globalization.CultureInfo]::InvariantCulture, [ref]$numeric)) {
        if ($numeric -gt 20000 -and $numeric -lt 90000) {
            return [DateTime]::FromOADate($numeric).ToString('dd/MM/yyyy')
        }
    }
    $Value
}

function Escape-Xml([string]$Value) {
    [Security.SecurityElement]::Escape($Value)
}

function ConvertTo-XlsxCellRef([int]$Column, [int]$Row) {
    $name = ''
    $current = $Column
    while ($current -gt 0) {
        $current--
        $name = [char]([int](65 + ($current % 26))) + $name
        $current = [math]::Floor($current / 26)
    }
    "$name$Row"
}

function Write-Xlsx([string]$Path, [string[]]$Headers, [object[]]$Rows) {
    if (Test-Path $Path) { Remove-Item -LiteralPath $Path }

    $temp = Join-Path ([IO.Path]::GetTempPath()) ([Guid]::NewGuid().ToString())
    New-Item -ItemType Directory -Path $temp | Out-Null
    try {
        New-Item -ItemType Directory -Path (Join-Path $temp '_rels') | Out-Null
        New-Item -ItemType Directory -Path (Join-Path $temp 'xl\_rels') | Out-Null
        New-Item -ItemType Directory -Path (Join-Path $temp 'xl\worksheets') | Out-Null

        Set-Content -LiteralPath (Join-Path $temp '[Content_Types].xml') -Encoding UTF8 -Value @'
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">
  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>
  <Default Extension="xml" ContentType="application/xml"/>
  <Override PartName="/xl/workbook.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml"/>
  <Override PartName="/xl/worksheets/sheet1.xml" ContentType="application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml"/>
</Types>
'@
        Set-Content -LiteralPath (Join-Path $temp '_rels\.rels') -Encoding UTF8 -Value @'
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument" Target="xl/workbook.xml"/>
</Relationships>
'@
        Set-Content -LiteralPath (Join-Path $temp 'xl\workbook.xml') -Encoding UTF8 -Value @'
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<workbook xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main" xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships">
  <sheets>
    <sheet name="Base compilada" sheetId="1" r:id="rId1"/>
  </sheets>
</workbook>
'@
        Set-Content -LiteralPath (Join-Path $temp 'xl\_rels\workbook.xml.rels') -Encoding UTF8 -Value @'
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">
  <Relationship Id="rId1" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet" Target="worksheets/sheet1.xml"/>
</Relationships>
'@

        $rowCount = $Rows.Count + 1
        $colCount = $Headers.Count
        $lastRef = ConvertTo-XlsxCellRef $colCount $rowCount
        $sheetBuilder = New-Object Text.StringBuilder
        [void]$sheetBuilder.AppendLine('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>')
        [void]$sheetBuilder.AppendLine('<worksheet xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main">')
        [void]$sheetBuilder.AppendLine("  <dimension ref=`"A1:$lastRef`"/>")
        [void]$sheetBuilder.AppendLine('  <sheetData>')

        [void]$sheetBuilder.AppendLine('    <row r="1">')
        for ($c = 1; $c -le $Headers.Count; $c++) {
            $ref = ConvertTo-XlsxCellRef $c 1
            [void]$sheetBuilder.AppendLine("      <c r=`"$ref`" t=`"inlineStr`"><is><t>$(Escape-Xml $Headers[$c - 1])</t></is></c>")
        }
        [void]$sheetBuilder.AppendLine('    </row>')

        for ($r = 0; $r -lt $Rows.Count; $r++) {
            $excelRow = $r + 2
            [void]$sheetBuilder.AppendLine("    <row r=`"$excelRow`">")
            for ($c = 1; $c -le $Headers.Count; $c++) {
                $header = $Headers[$c - 1]
                $value = [string]$Rows[$r].$header
                if ($value.Length -gt 0) {
                    $ref = ConvertTo-XlsxCellRef $c $excelRow
                    [void]$sheetBuilder.AppendLine("      <c r=`"$ref`" t=`"inlineStr`"><is><t>$(Escape-Xml $value)</t></is></c>")
                }
            }
            [void]$sheetBuilder.AppendLine('    </row>')
        }

        [void]$sheetBuilder.AppendLine('  </sheetData>')
        [void]$sheetBuilder.AppendLine('</worksheet>')
        Set-Content -LiteralPath (Join-Path $temp 'xl\worksheets\sheet1.xml') -Encoding UTF8 -Value $sheetBuilder.ToString()

        $zipStream = [IO.File]::Open($Path, [IO.FileMode]::CreateNew)
        $archive = New-Object IO.Compression.ZipArchive($zipStream, [IO.Compression.ZipArchiveMode]::Create)
        try {
            foreach ($file in Get-ChildItem -LiteralPath $temp -Recurse -File) {
                $relative = $file.FullName.Substring($temp.Length + 1).Replace('\', '/')
                $entry = $archive.CreateEntry($relative)
                $entryStream = $entry.Open()
                $fileStream = [IO.File]::OpenRead($file.FullName)
                try { $fileStream.CopyTo($entryStream) }
                finally {
                    $fileStream.Dispose()
                    $entryStream.Dispose()
                }
            }
        }
        finally {
            $archive.Dispose()
            $zipStream.Dispose()
        }
    }
    finally {
        if (Test-Path $temp) { Remove-Item -LiteralPath $temp -Recurse -Force }
    }
}

$headerMap = @{
    'numero de operacao' = 'Numero de operacao'
    'cliente' = 'Cliente'
    'cnpj' = 'CNPJ/CPF'
    'cpf/cnpj' = 'CNPJ/CPF'
    'descricao do projeto' = 'Descricao do projeto'
    'uf' = 'UF'
    'municipio' = 'Municipio'
    'municipio - codigo' = 'Codigo municipio'
    'numero do contrato' = 'Numero do contrato'
    'data da contratacao' = 'Data da contratacao'
    'valor contratado r$' = 'Valor contratado R$'
    'valor da operacao em r$' = 'Valor contratado R$'
    'valor desembolsado r$' = 'Valor desembolsado R$'
    'fonte de recurso (desembolsos)' = 'Fonte de recurso (desembolsos)'
    'custo financeiro' = 'Custo financeiro'
    'juros' = 'Juros'
    'prazo - carencia (meses)' = 'Prazo - carencia (meses)'
    'prazo - amortizacao (meses)' = 'Prazo - amortizacao (meses)'
    'modalidade de apoio' = 'Modalidade de apoio'
    'forma de apoio' = 'Forma de apoio'
    'produto' = 'Produto'
    'instrumento financeiro' = 'Instrumento financeiro'
    'inovacao' = 'Inovacao'
    'area operacional' = 'Area operacional'
    'setor cnae' = 'Setor CNAE'
    'subsetor cnae agrupado' = 'Subsetor CNAE agrupado'
    'subsetor cnae - codigo' = 'Subsetor CNAE - codigo'
    'subsetor cnae - nome' = 'Subsetor CNAE - nome'
    'setor bndes' = 'Setor BNDES'
    'subsetor bndes' = 'Subsetor BNDES'
    'porte do cliente' = 'Porte do cliente'
    'natureza do cliente' = 'Natureza do cliente'
    'instituicao financeira credenciada' = 'Instituicao financeira credenciada'
    'cnpj da instituicao financeira credenciada' = 'CNPJ da instituicao financeira credenciada'
    'cnpj do agente financeiro' = 'CNPJ da instituicao financeira credenciada'
    'tipo de garantia' = 'Tipo de garantia'
    'tipo de excepcionalidade' = 'Tipo de excepcionalidade'
    'situacao do contrato' = 'Situacao'
    'situacao da operacao' = 'Situacao'
}

$standardHeaders = @(
    'Tipo da operacao',
    'Origem da operacao',
    'Aba de origem',
    'Numero de operacao',
    'Numero do contrato',
    'Cliente',
    'CNPJ/CPF',
    'Descricao do projeto',
    'UF',
    'Municipio',
    'Codigo municipio',
    'Data da contratacao',
    'Valor contratado R$',
    'Valor desembolsado R$',
    'Fonte de recurso (desembolsos)',
    'Custo financeiro',
    'Juros',
    'Prazo - carencia (meses)',
    'Prazo - amortizacao (meses)',
    'Modalidade de apoio',
    'Forma de apoio',
    'Produto',
    'Instrumento financeiro',
    'Inovacao',
    'Area operacional',
    'Setor CNAE',
    'Subsetor CNAE agrupado',
    'Subsetor CNAE - codigo',
    'Subsetor CNAE - nome',
    'Setor BNDES',
    'Subsetor BNDES',
    'Porte do cliente',
    'Natureza do cliente',
    'Instituicao financeira credenciada',
    'CNPJ da instituicao financeira credenciada',
    'Tipo de garantia',
    'Tipo de excepcionalidade',
    'Situacao'
)

$inputStream = [IO.File]::Open((Resolve-Path $InputPath), [IO.FileMode]::Open, [IO.FileAccess]::Read, [IO.FileShare]::ReadWrite)
$zip = New-Object IO.Compression.ZipArchive($inputStream, [IO.Compression.ZipArchiveMode]::Read)
try {
    $sharedStrings = @()
    $sharedXml = Read-ZipEntryXml $zip 'xl/sharedStrings.xml'
    if ($sharedXml) {
        $sharedNs = New-Object Xml.XmlNamespaceManager($sharedXml.NameTable)
        $sharedNs.AddNamespace('m', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')
        foreach ($item in $sharedXml.SelectNodes('//m:si', $sharedNs)) {
            $sharedStrings += (($item.SelectNodes('.//m:t', $sharedNs) | ForEach-Object { $_.'#text' }) -join '')
        }
    }

    $workbook = Read-ZipEntryXml $zip 'xl/workbook.xml'
    $relationships = Read-ZipEntryXml $zip 'xl/_rels/workbook.xml.rels'
    $workbookNs = New-Object Xml.XmlNamespaceManager($workbook.NameTable)
    $workbookNs.AddNamespace('m', 'http://schemas.openxmlformats.org/spreadsheetml/2006/main')
    $workbookNs.AddNamespace('r', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')

    $relationshipById = @{}
    foreach ($rel in $relationships.Relationships.Relationship) {
        $relationshipById[$rel.Id] = $rel.Target
    }

    $compiledRows = New-Object Collections.Generic.List[object]
    foreach ($sheet in $workbook.SelectNodes('//m:sheet', $workbookNs)) {
        $sheetName = $sheet.name
        $relationshipId = $sheet.GetAttribute('id', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')
        $rows = @(Get-RowsFromSheet $zip $relationshipById[$relationshipId] $sharedStrings)

        $headerRow = $rows |
            Where-Object { ($_.Values | Where-Object { $_ -and $_.Trim().Length -gt 0 }).Count -ge 10 } |
            Select-Object -First 1

        if ($null -eq $headerRow) {
            throw "Cabecalho nao encontrado na aba $sheetName."
        }

        $columnMap = @{}
        for ($i = 0; $i -lt $headerRow.Values.Count; $i++) {
            $rawHeader = $headerRow.Values[$i]
            $key = Normalize-Key $rawHeader
            if ($headerMap.ContainsKey($key)) {
                $columnMap[$i] = $headerMap[$key]
            }
        }

        foreach ($row in ($rows | Where-Object { $_.RowNumber -gt $headerRow.RowNumber })) {
            $nonEmptyValues = @($row.Values | Where-Object { $_ -and $_.Trim().Length -gt 0 })
            if ($nonEmptyValues.Count -lt 3) { continue }

            $record = [ordered]@{}
            foreach ($header in $standardHeaders) { $record[$header] = '' }
            $record['Aba de origem'] = $sheetName

            for ($i = 0; $i -lt $row.Values.Count; $i++) {
                if (-not $columnMap.ContainsKey($i)) { continue }
                $target = $columnMap[$i]
                $value = $row.Values[$i].Trim()
                if ($target -eq 'Data da contratacao') {
                    $value = Convert-ExcelDate $value
                }
                $record[$target] = $value
            }

            $forma = (Remove-Accents $record['Forma de apoio']).ToUpperInvariant()
            if ($forma -like '*INDIRETA*' -or $sheetName -like 'Indiretas*') {
                $record['Tipo da operacao'] = 'Indireta'
                if ($sheetName -like 'Indiretas Autom*') {
                    $record['Origem da operacao'] = 'Indireta automatica'
                }
                else {
                    $record['Origem da operacao'] = 'Indireta nao automatica'
                }
            }
            else {
                $record['Tipo da operacao'] = 'Direta'
                $record['Origem da operacao'] = 'Direta'
            }

            $compiledRows.Add([pscustomobject]$record)
        }
    }
}
finally {
    $zip.Dispose()
    $inputStream.Dispose()
}

$compiledRows | Export-Csv -LiteralPath $CsvPath -NoTypeInformation -Delimiter ';' -Encoding UTF8
Write-Xlsx -Path $XlsxPath -Headers $standardHeaders -Rows $compiledRows.ToArray()

$counts = $compiledRows | Group-Object 'Origem da operacao' | Sort-Object Name | ForEach-Object { "$($_.Name): $($_.Count)" }
Write-Output "Arquivo CSV: $CsvPath"
Write-Output "Arquivo XLSX: $XlsxPath"
Write-Output "Total de linhas: $($compiledRows.Count)"
Write-Output "Contagem por origem:"
$counts | ForEach-Object { Write-Output $_ }
