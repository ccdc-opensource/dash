sudo DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends curl ca-certificates gpg apt-transport-https

# repository to install Intel(R) oneAPI Libraries
curl -fsSL https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2023.PUB | gpg --dearmor - | sudo tee /usr/share/keyrings/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2023.gpg >/dev/null
echo "deb [signed-by=/usr/share/keyrings/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2023.gpg] https://apt.repos.intel.com/oneapi all main" | sudo tee /etc/apt/sources.list.d/oneAPI.list

# repository to install up to date cmake
curl -fsSL https://apt.kitware.com/keys/kitware-archive-latest.asc | gpg --dearmor - | sudo tee /usr/share/keyrings/kitware-archive-keyring.gpg >/dev/null
echo "deb [signed-by=/usr/share/keyrings/kitware-archive-keyring.gpg] https://apt.kitware.com/ubuntu/ focal main" | sudo tee /etc/apt/sources.list.d/kitware.list

# repository to install vscode
curl -fsSL https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor - | sudo tee /etc/apt/trusted.gpg.d/packages.microsoft.gpg >/dev/null
echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/trusted.gpg.d/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" | sudo tee /etc/apt/sources.list.d/vscode.list

sudo apt-get update

# install dev libraries
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    build-essential pkg-config \
    libx11-dev libxss-dev libxxf86vm-dev libxkbfile-dev libxv-dev libmotif-dev libxmu-dev libxinerama-dev

# install tools
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    openssh-client git ninja-build cmake code libxshmfence1

# install compilers
sudo DEBIAN_FRONTEND=noninteractive apt-get install -y \
    intel-oneapi-compiler-fortran gfortran 

su - vagrant -c 'code --install-extension ms-vscode.cpptools'
su - vagrant -c 'code --install-extension ms-vscode.cmake-tools'
su - vagrant -c 'code --install-extension krvajalm.linter-gfortran'
su - vagrant -c 'code --install-extension mhutchie.git-graph'
