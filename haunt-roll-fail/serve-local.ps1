param(
    [int] $Port = 8080,
    [string] $BindAddress = "127.0.0.1"
)

$ErrorActionPreference = "Stop"
$root = [IO.Path]::GetFullPath($PSScriptRoot)
$listener = New-Object System.Net.HttpListener
$bindHost = $BindAddress
if ($BindAddress -eq "0.0.0.0") { $bindHost = "+" }
$prefix = "http://${bindHost}:${Port}/"
$listener.Prefixes.Add($prefix)

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
    Write-Host "Failed to open port ${Port}: $_"
    Write-Host "Run once as Administrator:"
    Write-Host "  netsh http add urlacl url=$prefix user=$env:USERNAME"
    exit 1
}

Write-Host "Root: $root"
if ($bindHost -eq "+") {
    Write-Host "Listening on all interfaces: $prefix"
    Write-Host "This PC: http://127.0.0.1:${Port}/index.html"
    Write-Host "LAN: http://<LAN-IP>:${Port}/index.html"
} else {
    Write-Host "Open in browser: http://${BindAddress}:${Port}/index.html"
}
Write-Host "Stop: Ctrl+C"
Write-Host ""

try {
    while ($listener.IsListening) {
        $ctx = $listener.GetContext()
        $req = $ctx.Request
        $res = $ctx.Response
        $rel = [Uri]::UnescapeDataString($req.Url.AbsolutePath.TrimStart("/"))
        if ([string]::IsNullOrEmpty($rel)) { $rel = "index.html" }
        if ($rel.StartsWith("hrf/")) { $rel = $rel.Substring(4) }
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
