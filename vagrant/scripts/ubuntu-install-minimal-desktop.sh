if [[ "$(dpkg -s ubuntu-desktop-minimal | grep Status)" != "Status: install ok installed" ]]
then
    echo
    echo "minimal desktop not installed, installing (might take a while)"
    sudo DEBIAN_FRONTEND=noninteractive apt-get install -y ubuntu-desktop-minimal
    sudo reboot
fi
