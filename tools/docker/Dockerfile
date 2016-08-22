# We want `ocaml`, `opam`, and the `biokepi` user:
FROM hammerlab/biokepi-run

# `opam` is the user with `sudo` powers
USER opam
ENV HOME /home/opam
WORKDIR /home/opam

ENV CLOUDSDK_CORE_DISABLE_PROMPTS true
RUN bash -c 'curl https://sdk.cloud.google.com | bash'

RUN sudo apt-get install zlib1g-dev
RUN opam install --yes tlstunnel
RUN opam pin add solvuu-build https://github.com/solvuu/solvuu-build.git
RUN opam pin add coclobas https://github.com/hammerlab/coclobas.git
