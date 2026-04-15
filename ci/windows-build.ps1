Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

param(
  [Parameter(Mandatory = $true)]
  [string]$Project,

  [ValidateSet('Win32', 'Win64')]
  [string]$Platform = 'Win64',

  [string]$BdsVersion = $(if ($env:BDS_VERSION) { $env:BDS_VERSION } else { '23.0' }),

  [string]$BuildDir
)

$repoRoot = Split-Path -Parent (Split-Path -Parent $PSCommandPath)
$projectPath = Join-Path $repoRoot $Project

if (-not (Test-Path -LiteralPath $projectPath)) {
  throw "Project not found: $Project"
}

if (-not $BuildDir) {
  $BuildDir = Join-Path $repoRoot "Bin\$Platform"
}

$sourceDir = Join-Path $repoRoot 'Source'

if ($Platform -eq 'Win32') {
  $compiler = if ($env:DCC32) { $env:DCC32 } else { "C:\Program Files (x86)\Embarcadero\Studio\$BdsVersion\bin\dcc32.exe" }
  $delphiLib = if ($env:DELPHI_LIB_WIN32) { $env:DELPHI_LIB_WIN32 } else { "C:\Program Files (x86)\Embarcadero\Studio\$BdsVersion\lib\win32\release" }
  $runtimeDllName = 'lua55.dll'
} else {
  $compiler = if ($env:DCC64) { $env:DCC64 } else { "C:\Program Files (x86)\Embarcadero\Studio\$BdsVersion\bin\dcc64.exe" }
  $delphiLib = if ($env:DELPHI_LIB_WIN64) { $env:DELPHI_LIB_WIN64 } else { "C:\Program Files (x86)\Embarcadero\Studio\$BdsVersion\lib\win64\release" }
  $runtimeDllName = 'lua55_64.dll'
}

if (-not (Test-Path -LiteralPath $compiler)) {
  throw "Compiler not found: $compiler"
}

$runtimeSource = Join-Path (Join-Path (Join-Path $repoRoot 'Runtime') $Platform) $runtimeDllName
if (-not (Test-Path -LiteralPath $runtimeSource)) {
  throw "Runtime DLL not found: $runtimeSource"
}

New-Item -ItemType Directory -Force -Path $BuildDir | Out-Null
$runtimeTarget = Join-Path $BuildDir $runtimeDllName

$searchPath = "$sourceDir;$delphiLib"
$projectDir = Split-Path -Parent $projectPath
$projectFile = Split-Path -Leaf $projectPath

Push-Location $projectDir
try {
  $compilerArgs = @(
    '-B'
    "-U$searchPath"
    "-I$sourceDir"
    "-N0$BuildDir"
    "-NU$BuildDir"
    "-DPLATFORM_$Platform"
    "-E$BuildDir"
    $projectFile
  )

  & $compiler @compilerArgs
  if ($LASTEXITCODE -ne 0) {
    throw "Compiler failed for $Project ($Platform) with exit code $LASTEXITCODE."
  }
} finally {
  Pop-Location
}

Copy-Item -LiteralPath $runtimeSource -Destination $runtimeTarget -Force
Write-Host "Built $(Split-Path -LeafBase $projectPath) for $Platform in $BuildDir"
