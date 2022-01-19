$intel_base_installer_url="https://registrationcenter-download.intel.com/akdlm/irc_nas/18195/w_BaseKit_p_2021.4.0.3421.exe"
$intel_hpc_installer_url="https://registrationcenter-download.intel.com/akdlm/irc_nas/18247/w_HPCKit_p_2021.4.0.3340.exe"

if (-not( Test-Path C:\intel-installers))
{
    New-Item -Path C:\ -name "intel-installers" -type directory
}

$intel_base_installer_localfile="C:\intel-installers\$(Split-Path -Leaf $intel_base_installer_url)"
if (-not( Test-Path $intel_base_installer_localfile ))
{
    Write-Host "Fetching intel oneAPI base installer"
    Invoke-WebRequest -Uri $intel_base_installer_url -OutFile $intel_base_installer_localfile
}
if (-not( Test-Path "C:/Program Files (x86)/Intel/oneAPI/dev-utilities/latest" ))
{
    Write-Host "Installing intel oneAPI base installer"
    & $intel_base_installer_localfile `
    --silent `
    --remove-extracted-files yes `
    --a `
    --silent `
    --eula=accept `
    --components=intel.oneapi.win.dpcpp_debugger:intel.oneapi.win.vtune `
    --log-dir=. `
    --intel-sw-improvement-program-consent=no `
    "--install-dir=C:/Program Files (x86)/Intel/oneAPI"
}

$intel_hpc_installer_localfile="C:\intel-installers\$(Split-Path -Leaf $intel_hpc_installer_url)"
if (-not( Test-Path $intel_hpc_installer_localfile ))
{
    Write-Host "Fetching intel oneAPI hpc installer"
    Invoke-WebRequest -Uri $intel_hpc_installer_url -OutFile $intel_hpc_installer_localfile
}
if (-not( Test-Path "C:/Program Files (x86)/Intel/oneAPI/compiler/latest" ))
{
    Write-Host "Installing intel oneAPI hpc installer"
    & $intel_hpc_installer_localfile `
    --remove-extracted-files no `
    --a `
    --silent `
    --eula=accept `
    --components=intel.oneapi.win.ifort-compiler `
    --intel-sw-improvement-program-consent=no
}
