Param(
  [string]$Version = "latest",
  [string]$InstallDir = ""
)

$ErrorActionPreference = "Stop"

$Repo = "guywaldman/glue"

if ([string]::IsNullOrWhiteSpace($InstallDir)) {
  $InstallDir = Join-Path $HOME ".local\bin"
}

$arch = $env:PROCESSOR_ARCHITECTURE
switch ($arch) {
  "AMD64" { $arch = "amd64" }
  "ARM64" { $arch = "arm64" }
  default { throw "Unsupported architecture: $($env:PROCESSOR_ARCHITECTURE)" }
}

if ($Version -ne "latest" -and -not $Version.StartsWith("v")) {
  $Version = "v$Version"
}

if ($Version -eq "latest") {
  $tarballUrl = "https://github.com/$Repo/releases/latest/download/glue_windows_$arch.tar.gz"
} else {
  $tarballUrl = "https://github.com/$Repo/releases/download/$Version/glue_windows_$arch.tar.gz"
}

Write-Host "Downloading $tarballUrl" -ForegroundColor Cyan

$tempDir = Join-Path ([System.IO.Path]::GetTempPath()) ([System.Guid]::NewGuid().ToString("n"))
New-Item -ItemType Directory -Force -Path $tempDir | Out-Null

$tgzPath = Join-Path $tempDir "glue.tgz"
Invoke-WebRequest -Uri $tarballUrl -OutFile $tgzPath

if (-not (Get-Command tar -ErrorAction SilentlyContinue)) {
  throw "'tar' was not found. Install a recent Windows version (tar is built-in on Windows 10/11) or extract the .tar.gz manually."
}

tar -xzf $tgzPath -C $tempDir | Out-Null

$src = Join-Path $tempDir "glue.exe"
if (-not (Test-Path $src)) {
  throw "Expected glue.exe in archive, but it was not found"
}

New-Item -ItemType Directory -Force -Path $InstallDir | Out-Null
$dest = Join-Path $InstallDir "glue.exe"
Copy-Item -Force $src $dest

Write-Host "Installed glue to $dest" -ForegroundColor Green
Write-Host "If 'glue' isn't found, add $InstallDir to your PATH." -ForegroundColor Yellow
