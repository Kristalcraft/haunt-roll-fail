param(
    [switch] $MirrorWebp2
)

$ErrorActionPreference = "Stop"
$root = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
$srcRoot = Join-Path $root "vast\assets"

if (-not (Test-Path -LiteralPath $srcRoot)) {
    Write-Error "Missing folder: $srcRoot"
}

if ($MirrorWebp2) {
    $nested = Join-Path $srcRoot "webp2"
    if (-not (Test-Path -LiteralPath $nested)) {
        Write-Error "Expected vast\assets\webp2\ tree, or omit -MirrorWebp2"
    }
    $destWebp2 = Join-Path $root "webp2"
    Write-Host "Copy $nested -> $destWebp2"
    New-Item -ItemType Directory -Force -Path $destWebp2 | Out-Null
    Copy-Item -Path (Join-Path $nested "*") -Destination $destWebp2 -Recurse -Force
    Write-Host "Done."
    exit 0
}

$subdirs = @("icons", "figures", "tiles", "board", "cards")
$destImages = Join-Path $root "webp2\vast\images"
$copied = 0

foreach ($sub in $subdirs) {
    $from = Join-Path $srcRoot $sub
    if (-not (Test-Path -LiteralPath $from)) {
        Write-Warning "Skip (no folder): $from"
        continue
    }
    $to = Join-Path $destImages $sub
    New-Item -ItemType Directory -Force -Path $to | Out-Null
    Get-ChildItem -LiteralPath $from -File | ForEach-Object {
        Copy-Item -LiteralPath $_.FullName -Destination (Join-Path $to $_.Name) -Force
        $script:copied++
    }
    Write-Host "OK $sub"
}

Write-Host ""
Write-Host "Copied $copied files -> $destImages"
if ($copied -eq 0) {
    Write-Warning "Try: .\scripts\sync-vast-assets.ps1 -MirrorWebp2"
}
