param(
    [int] $Port = 8080
)

$ErrorActionPreference = "Stop"
$root = [IO.Path]::GetFullPath($PSScriptRoot)
$listener = New-Object System.Net.HttpListener
$listener.Prefixes.Add("http://127.0.0.1:$Port/")

function Get-Mime([string] $ext) {
    switch ($ext.ToLowerInvariant()) {
        ".html" { return "text/html; charset=utf-8" }
        ".js" { return "application/javascript; charset=utf-8" }
        ".mjs" { return "application/javascript; charset=utf-8" }
        ".css" { return "text/css; charset=utf-8" }
        ".json" { return "application/json; charset=utf-8" }
        ".woff" { return "font/woff" }
        ".woff2" { return "font/woff2" }
        ".png" { return "image/png" }
        ".jpg" { return "image/jpeg" }
        ".jpeg" { return "image/jpeg" }
        ".gif" { return "image/gif" }
        ".svg" { return "image/svg+xml" }
        ".ico" { return "image/x-icon" }
        ".map" { return "application/json" }
        ".wasm" { return "application/wasm" }
        default { return "application/octet-stream" }
    }
}

try {
    $listener.Start()
} catch {
    Write-Host "Не удалось открыть порт $Port : $_"
    Write-Host "Один раз от администратора можно выполнить:"
    Write-Host "  netsh http add urlacl url=http://127.0.0.1:$Port/ user=$env:USERNAME"
    exit 1
}

Write-Host "Каталог: $root"
Write-Host "Открой в браузере: http://127.0.0.1:$Port/index.html"
Write-Host "Остановка: Ctrl+C"
Write-Host ""

try {
    while ($listener.IsListening) {
        $ctx = $listener.GetContext()
        $req = $ctx.Request
        $res = $ctx.Response
        $rel = [Uri]::UnescapeDataString($req.Url.AbsolutePath.TrimStart("/"))
        if ([string]::IsNullOrEmpty($rel)) { $rel = "index.html" }
        $rel = $rel -replace "/", [IO.Path]::DirectorySeparatorChar
        $full = [IO.Path]::GetFullPath([IO.Path]::Combine($root, $rel))
        if (-not $full.StartsWith($root, [StringComparison]::OrdinalIgnoreCase)) {
            $res.StatusCode = 403
            $res.Close()
            continue
        }
        if (Test-Path -LiteralPath $full -PathType Container) {
            $full = [IO.Path]::Combine($full, "index.html")
        }
        if (-not (Test-Path -LiteralPath $full -PathType Leaf)) {
            $res.StatusCode = 404
            $msg = [Text.Encoding]::UTF8.GetBytes("404")
            $res.ContentLength64 = $msg.LongLength
            $res.OutputStream.Write($msg, 0, $msg.Length)
            $res.Close()
            continue
        }
        $bytes = [IO.File]::ReadAllBytes($full)
        $res.ContentType = Get-Mime ([IO.Path]::GetExtension($full))
        $res.ContentLength64 = $bytes.LongLength
        $res.OutputStream.Write($bytes, 0, $bytes.Length)
        $res.Close()
    }
} finally {
    $listener.Stop()
    $listener.Close()
}
