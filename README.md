Coclobas
======

Coclobas provides an OCaml interface on top of Kubernetes, making it easy to
programatically interface with a cluster from the command-line and from code. It
provides logging facilities, maintaining logs long after Kubernetes disposes of
them. Finally, it makes it easy to submit arbitrary scripts to be run in any
Docker container, which makes it easier than using raw Kubernetes to submit
arbitrarily complex jobs.

Right now Coclobas is Kubernetes focused and works with Google Container Engine,
but we aim to support arbitrary Kubernetes deployments, as well as alternate
scheduler types.


Build
-----

Coclobas uses the development version of Solvuu-build:

    opam pin add solvuu-build https://github.com/solvuu/solvuu-build.git

Then, clone this repo and:

    make
    
After pinning `solvuu-build`, you can also just `opam pin add coclobas
https://github.com/hammerlab/coclobas.git` to get going quickly.

Requirements
-----

`kubectl` must be installed local to the Coclobas cluster, as it is shelled out
to in order to query the Kubernetes cluster being used to run the submitted
job. The [`gcloud` utilities](https://cloud.google.com/sdk/gcloud/) must also be
installed and initialized for automatic cluster management.


Using Coclobas
-----

You will need to briefly configure Coclobas prior to using it. This is mostly a
matter of choosing a directory (`--root`) for the Coclobas database to live in,
and telling it where it can find your Kubernetes cluster (`--cluster-name`) on
the Google Container Engine (Coclobas will make a cluster if one doesn't already
exist with that name) and which compute zone that the cluster is in
(`--cluser-zone`). Finally, you'll want to limit the total number of pods
Coclobas can schedule (`--max-nodes`).

```shell
coclobas configure --root _configure_root/ \
    --cluster-name some-cluster \
    --cluster-zone us-east1-c \ # Zone 
    --max-nodes 20  # The total number pods Coclobas will schedule at one time

```

Now that Coclobas has a configuration (this information is store in the
`--root`, so you can have many possible Coclobas configured, all in different
directories), you'll want to start it:

```shell
coclobas start-server --root _configure_coco --port 8999
```

You can visit this at 127.0.0.1:8999/status to see if Coclobas is ready to go.

Now you should be able to submit jobs to your Coclobas server, and have them
scheduled on GCE.
