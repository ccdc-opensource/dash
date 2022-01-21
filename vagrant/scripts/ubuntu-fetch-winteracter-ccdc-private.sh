if [ ! -f /opt/winteracter/winteracter-linux-14.10d/bin/rc ]
then
    echo "Fething winteracter from repository"
    ARTIFACTORY_API_KEY=$1
    mkdir -p /opt/winteracter
    pushd /opt/winteracter
    curl -H "X-JFrog-Art-Api:$ARTIFACTORY_API_KEY" -LO "https://artifactory.ccdc.cam.ac.uk/artifactory/dash-private-dependencies/winteracter-linux-14.10d.tar.bz2"
    curl -H "X-JFrog-Art-Api:$ARTIFACTORY_API_KEY" -LO "https://artifactory.ccdc.cam.ac.uk/artifactory/dash-private-dependencies/xwint_rc_14_10e.tar.gz"
    tar jxf winteracter-linux-14.10d.tar.bz2
    pushd winteracter-linux-14.10d
    tar zxf ../xwint_rc_14_10e.tar.gz
fi