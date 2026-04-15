<#
.SYNOPSIS
  Строит список путей .webp для Vast по vast/meta.scala (как в HRFR.startGame).

.DESCRIPTION
  Клиент грузит файлы по относительным URL:
    webp2/vast/images/<подпапка>/<имя>.webp
  Подпапки: icons, figures, tiles, board, cards — из ConditionalAssetsList в meta.scala.

  Использование:
    .\scripts\list-vast-assets.ps1                    # печать в консоль
    .\scripts\list-vast-assets.ps1 -OutFile list.txt
    .\scripts\list-vast-assets.ps1 -BaseUrl "https://example.com/hrf/"  # + полные URL (для справки)

  Корень скрипта — каталог haunt-roll-fail (где лежат vast/, index.html).
#>
param(
    [string] $OutFile = "",
    [string] $BaseUrl = ""
)

$ErrorActionPreference = "Stop"
$here = Split-Path -Parent $MyInvocation.MyCommand.Path
$root = Split-Path -Parent $here
$metaFile = Join-Path $root "vast\meta.scala"

if (-not (Test-Path -LiteralPath $metaFile)) {
    Write-Error "Не найден $metaFile"
}

$lines = Get-Content -LiteralPath $metaFile -Encoding UTF8
$currentDir = $null
$relPaths = [System.Collections.Generic.List[string]]::new()

foreach ($line in $lines) {
    # Первый аргумент ConditionalAssetsList содержит запятые ($[Faction], ...), поэтому не парсим скобки жадно.
    if ($line -match 'ConditionalAssetsList\(.+,\s*"([^"]+)"\s*\)\s*\(') {
        $currentDir = $Matches[1]
        continue
    }
    if ($null -ne $currentDir -and $line -match 'ImageAsset\(\s*"([^"]+)"') {
        $name = $Matches[1]
        $rel = "webp2/vast/images/$currentDir/$name.webp"
        $relPaths.Add($rel)
    }
}

$relPaths.Sort()
$unique = $relPaths | Select-Object -Unique

if ($BaseUrl -ne "") {
    $u = $BaseUrl.TrimEnd("/") + "/"
    $rows = $unique | ForEach-Object { [pscustomobject]@{ Relative = $_; Url = ($u + $_ -replace '\\', '/') } }
} else {
    $rows = $unique | ForEach-Object { [pscustomobject]@{ Relative = $_ } }
}

Write-Host "Vast: $($unique.Count) ассетов (уникальных путей webp2/...)" -ForegroundColor Cyan

if ($OutFile -ne "") {
    $outPath = if ([System.IO.Path]::IsPathRooted($OutFile)) { $OutFile } else { Join-Path $root $OutFile }
    $unique | Set-Content -LiteralPath $outPath -Encoding UTF8
    Write-Host "Записано: $outPath"
}

$rows | Format-Table -AutoSize
