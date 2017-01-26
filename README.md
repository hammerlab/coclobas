Coclobas
========

Coclobas is a scheduler for HPC-like jobs accessible through HTTP.

It can be setup with two kinds of configurations:

- Using Kubernetes and the *Google Container Engine*,
  i.e. using a Kubernetes “eleastic” cluster setup with `gcloud` and submitting
  jobs as Kubernetes “pods”.
- Using the server's machine as a one-node cluster and submitting jobs as docker
  containers given a maximal number of jobs.

Coclobas provides logging facilities (e.g. maintaining logs long after Kubernetes
disposes of them).

Finally, it makes it easy to submit arbitrary scripts to be run in any Docker
container, which makes it easier than using raw Kubernetes or Docker to submit
arbitrarily complex jobs.

If the `ketrew` package is present, Coclobas comes with a Ketrew plugin
(loadable dynamically)
and a build of the Ketrew binary with the plugin already loaded in (for
deployment convenience).


Build
-----

Coclobas 0.0.0 is on `opam` (supports only GKE/Kubernetes).

Coclobas depends on `solvuu-build`, `sosa`, `nonstd`, and optionally `ketrew`.

You can just use Opam to get things going quickly:

    opam pin add coclobas https://github.com/hammerlab/coclobas.git

Or you may clone this repo and:

    make

Requirements
------------

In GKE/Kubernetes mode, `gcloud` and `kubectl` 
must be installed (and authenticated) with the Coclobas server.

In Local/Docker mode, `docker` must be present (and accessible to the Coclobas
server's user).


Using Coclobas
--------------

### Configuration

You first need to create a “root” directory, see:

    coclobas configure --help
    
Example 1: GKE/Kubernetes mode:

    coclobas config --root $root \
             --cluster-kind gke \
             --gke-cluster-name "my-coclotest-cluster" \
             --gcloud-zone "us-east1-c" \
             --max-nodes 5

Example 2: Local/Docker mode:

    coclobas config --root $root \
             --cluster-kind local-doker \
             --max-nodes 5

### Start The Server

Now that Coclobas has a configuration (this information is store in the
`--root`, so you can have many possible Coclobas configured, all in different
directories), you'll want to start it:


    coclobas start-server --root $root --port 8999


You can use this `curl http://127.0.0.1:8999/status` to see if Coclobas is ready
to go (if it says `Initializing` usually it means that it is setting up a
GKE-cluster which takes a few minutes).

### Submitting Jobs

The most common way of submitting jobs is through the Ketrew plugin, see
examples in `src/test/workflow_test.ml`.

In GKE/Kubernetes mode:

```ocaml
workflow_node without_product ~name:"Coclobas that uses the GKE/Kubernetes"
  ~make:(
    Coclobas_ketrew_backend.Plugin.kubernetes_program
      ~base_url:"http://127.0.0.1:8999/"
      ~image:"ubuntu"
      ~volume_mounts:[
        `Nfs (
          Coclobas.Kube_job.Specification.Nfs_mount.make
            ~host:"nfs-server.example.com"
            ~path:"/path/inside/nfs-server"
            ~point:"/mount/point/" ())
      ]
      Program.(
        chain [
          shf "hostname";
          shf "du -sh /mount/point";
          shf "sleep 60";
        ]
      )
  )
```

In Local/Docker mode:

```ocaml
workflow_node without_product ~name:"Coclobas test of local-docker jobs"
  ~make:(
    Coclobas_ketrew_backend.Plugin.local_docker_program
      ~base_url:"http://127.0.0.1:8999/"
      ~image:"ubuntu"
      ~volume_mounts:[
        `Local ("/usr/bin", "/hostusrbin")
      ]
      Program.(
        exec ["find"; "/hostusrbin"]
      )
  )
```

Contact
-------

If you have any questions, you may submit an
[issue](https://github.com/hammerlab/coclobas/issues), or join
the authors on the public “Slack” channel of the Hammer Lab:
[![Slack Status](http://publicslack.hammerlab.org/badge.svg)](http://publicslack.hammerlab.org)

License
-------

It's [Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0).

