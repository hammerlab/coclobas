Docker Container To Play With Coclobas
======================================



So far:

    sudo docker pull hammerlab/coclobas:Add-Dockerfile
    sudo docker run -it -p 443:443 --privileged hammerlab/coclobas:Add-Dockerfile bash


- `--privileged` is for NFS mounting
- `-p 443:443` is to pass the port 443 to the container



Warnings
--------

The environment variable `CLOUDSDK_CORE_DISABLE_PROMPTS` is set to `true`; if
you want to get back the usual `gcloud` interactiveness you may want to:

    unset CLOUDSDK_CORE_DISABLE_PROMPTS

