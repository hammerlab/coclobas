# We want `ocaml`, `opam`, and the `biokepi` user:
FROM hammerlab/biokepi-run

# `opam` is the user with `sudo` powers
USER opam
ENV HOME /home/opam
WORKDIR /home/opam

ENV CLOUDSDK_CORE_DISABLE_PROMPTS true
RUN bash -c 'curl https://sdk.cloud.google.com | bash'

