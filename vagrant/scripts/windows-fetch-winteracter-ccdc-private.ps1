param(
    $ApiKey
)

$URL="https://artifactory.ccdc.cam.ac.uk/artifactory/dash-private-dependencies/winteracter-14.10d.7z"
$LocalZip="C:\winteracter\$(Split-Path -Leaf $URL)"

if (-not( Test-Path C:\winteracter))
{
    New-Item -Path C:\ -name "winteracter" -type directory
}
if (-not( Test-Path $LocalZip ))
{
    $headers = @{ "X-JFrog-Art-Api" = $ApiKey }
    Invoke-WebRequest -Uri $URL -Headers $headers -OutFile $LocalZip
}

if (-not( Test-Path C:\winteracter\winteracter-14.10d\bin))
{
    & "$env:ProgramFiles\7-Zip\7z.exe" x -oC:\winteracter $LocalZip -r
}
