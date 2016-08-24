Docker Container To Play With Coclobas
======================================


Usage
-----

Get the container ready:

    sudo docker pull hammerlab/coclobas
    sudo mkdir -p /tmp/coclo
    sudo chmod 777 /tmp/coclo
    sudo docker run -it -p 443:443  -v /tmp/coclo:/coclo --privileged hammerlab/coclobas bash

- `--privileged` is for NFS mounting
- `-p 443:443` is to pass the port 443 to the container

Put your `configuration.env` in `/tmp/coclo` (or `/coclo` if you edit it from
the container), and then if all goes well you can mount & start everything with:

    bash please.sh /coclo/configuration.env start_all
    


Warnings
--------

The environment variable `CLOUDSDK_CORE_DISABLE_PROMPTS` is set to `true`; if
you want to get back the usual `gcloud` interactiveness you may want to:

    unset CLOUDSDK_CORE_DISABLE_PROMPTS

