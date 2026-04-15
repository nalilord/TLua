Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

param(
  [string]$Project = 'Tests\TLuaTests.dpr',

  [ValidateSet('Win32', 'Win64')]
  [string]$Platform = 'Win64'
)

$repoRoot = Split-Path -Parent (Split-Path -Parent $PSCommandPath)
$projectName = [System.IO.Path]::GetFileNameWithoutExtension($Project)
$testExe = Join-Path $repoRoot "Bin\$Platform\$projectName.exe"

& (Join-Path $repoRoot 'ci\windows-build.ps1') -Project $Project -Platform $Platform

if (-not (Test-Path -LiteralPath $testExe)) {
  throw "Expected test executable was not produced: $testExe"
}

& $testExe
if ($LASTEXITCODE -ne 0) {
  throw "Test executable failed with exit code $LASTEXITCODE."
}
