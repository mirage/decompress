FROM ocaml/opam:debian-10-ocaml-4.13

RUN sudo apt-get install -qq -yy --no-install-recommends zlib1g-dev pkg-config

RUN opam exec -- opam remote add origin https://opam.ocaml.org

RUN opam exec -- opam update

COPY --chown=opam:opam . bench-dir

WORKDIR bench-dir

RUN opam exec -- opam install -y --deps-only ./decompress-bench.opam

