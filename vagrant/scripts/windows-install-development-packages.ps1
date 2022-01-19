choco install -y `
    git 7zip.install `
    ninja cmake `
    vscode visualstudio2019community visualstudio2019-workload-nativedesktop `
    wixtoolset

$vscode_extensions = 
    "ms-vscode.cpptools",
    "ms-vscode.cmake-tools",
    "krvajalm.linter-gfortran",
    "mhutchie.git-graph"

foreach ($vscode_extension in $vscode_extensions)
{
    & "C:\Program Files\Microsoft VS Code\bin\code.cmd" --install-extension $vscode_extension
}
