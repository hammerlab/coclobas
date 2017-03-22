#!/usr/bin/env bash

set -e

usage () {
    echo "Usage: bash please.sh <config-file> <command>"
    echo ""
    echo "where <command> may be:"
    echo ""
    echo "- start_all: mount_all + start screen session with everything"
    echo "- mount_all: mount all the NFS mounts"
    echo ""
}

if [ -f "$1" ]; then
    export PATH=$PATH:.
    export config_file=$1
    . $1
    shift
else
    echo "Argument \$1 should be a configuration file"
    usage
    exit 2
fi
case $1 in
    ""|help|"-h"|"--help" )
        usage
        exit 0;;
    * ) echo "Let's do this …" ;;
esac

if [ "$CLUSTER_NAME" = "" ] || [ "$CLUSTER_NAME" = "kocluster" ]; then
    echo "Error: \$CLUSTER_NAME is not set or has an unacceptable value: $CLUSTER_NAME"
    exit 3
fi
if [ "$GCLOUD_ZONE" = "" ] ; then
    echo "Error: \$GCLOUD_ZONE is not set"
    exit 3
fi
if [ "$TOKEN" = "" ] ; then
    echo "Error: \$TOKEN is not set"
    exit 3
fi
if [ "$CLUSTER_MAX_NODES" = "" ] ; then
    echo "Error: \$CLUSTER_MAX_NODES is not set"
    exit 3
fi
if [ "$CLUSTER_MACHINE_TYPE" = "" ] ; then
    CLUSTER_MACHINE_TYPE="n1-highmem-8"
fi
if [ "$NFS_MOUNTS" = "" ] ; then
    echo "Warning: \$MOUNT_NFS is not set, your cluster is going to be useless"
fi


mount_nfs () {
    local host=$1
    local storage=$2
    local witness=$3
    local mount_point=$4
    if [ "$1" = "" ] || [ "$2" = "" ] || [ "$3" = "" ] || [ "$4" = "" ] ; then
        echo "mount_nfs: invalid arguments"
        return 2
    fi
    if [ -f $mount_point/$witness ] ; then
        echo "mount_nfs: $host:$storage already mounted, cf. $mount_point/$witness"
        return
    fi
    sudo mkdir -p $mount_point
    sudo mount -t nfs $host:$storage $mount_point
    #sudo chmod -R 777 $mount_point
    cat $mount_point/$witness
}
mount_all () {
    for mo in `echo "$NFS_MOUNTS" | sed 's/:/\n/g'` ; do
        arguments=$(echo $mo | sed 's/,/ /g')
        echo "--> Mounting: $arguments"
        mount_nfs $arguments
    done
}

# This is now not used any more, kept around for a couple of commits …
make_biokepi_user(){
    sudo sh -c "adduser --uid 20042 --disabled-password --gecos '' biokepi && \
	passwd -l biokepi && \
	chown -R biokepi:biokepi /home/biokepi"
}

start_coclobas () {
    root=_cocloroot
    ccb=coclobas
    $ccb config --root $root \
         --cluster-name $CLUSTER_NAME \
         --cluster-zone $GCLOUD_ZONE \
         --max-nodes $CLUSTER_MAX_NODES \
         --machine-type $CLUSTER_MACHINE_TYPE
    export COCLOBAS_DEBUG_SECTIONS="/server/loop"
    $ccb start-server --root $root --port 8082
}

tls_config () {
    if [ -f _fake_tls/privkey-nopass.pem ] ; then
        echo "TLS cert/key already configured"
    else
        ketrew init --config _fake_tls --self-signed
    fi
}

start_ketrew () {
    sudo chmod -R 777 .
    sudo chmod -R 777 /tmp/ketrew
    ketrew init --config _ketrew_config --port 8080 --debug 1 --with-tok $TOKEN
    ketrew_bin=`which coclobas-ketrew`
    ketrew_config=_ketrew_config/config.json
    ocaml _ketrew_config/configuration.ml > $ketrew_config
    sudo chmod -R 777 _ketrew_config
    sudo su biokepi -c "KETREW_CONFIG=$ketrew_config $ketrew_bin start -P server"
}

start_tlstunnel () {
    tls_config
    tt=`which tlstunnel`
    sudo $tt --cert _fake_tls/certificate.pem \
         --key _fake_tls/privkey-nopass.pem \
         --backend 127.0.0.1:8080 --frontend :443
}

access_rights_on_console () {
    sudo chmod 777 /dev/console
}

start_all () {
    mount_all
    access_rights_on_console
    if [ -f ~/.screenrc ]; then
        cat ~/.screenrc > screenrc
    else
        echo '# Generated screenrc
hardstatus alwayslastline "%c   %-w [ %n %t ] %+w"
' > screenrc
    fi
    cat >> screenrc <<EOF
screen -t  Ketrew-server please.sh $config_file start_ketrew
screen -t Coclobas-server please.sh $config_file start_coclobas
screen -t Sudo-tlstunnel please.sh $config_file start_tlstunnel
screen bash
EOF
    screen -S ketrewcoclo -c screenrc
}

$*
