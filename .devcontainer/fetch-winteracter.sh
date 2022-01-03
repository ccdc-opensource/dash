if [[ "$ARTIFACTORY_API_KEY" != "" ]]
then
    echo "Fething winteracter from repository"
    mkdir -p /opt/winteracter
    pushd /opt/winteracter
    curl -H "X-JFrog-Art-Api:$ARTIFACTORY_API_KEY" -LO "https://artifactory.ccdc.cam.ac.uk/artifactory/dash-private-dependencies/winteracter-linux-14.10d.tar.bz2"
    tar jxf winteracter-linux-14.10d.tar.bz2
    curl -H "X-JFrog-Art-Api:$ARTIFACTORY_API_KEY" -LO "https://artifactory.ccdc.cam.ac.uk/artifactory/dash-private-dependencies/xwint_rc_14_10e.tar.gz"
    tar zxf xwint_rc_14_10e.tar.gz
fi