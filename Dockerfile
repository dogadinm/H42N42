FROM ocaml/opam:debian-12-ocaml-4.14

RUN sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    m4 pkg-config \
    zlib1g-dev \
    libgmp-dev libssl-dev libsqlite3-dev \
    && sudo rm -rf /var/lib/apt/lists/*

WORKDIR /home/opam/app

COPY --chown=opam:opam . .

RUN if [ -d src ]; then \
      cp -r src/* . && rm -rf src ; \
    fi


RUN opam update && opam install -y dune
RUN opam install -y . --deps-only
RUN opam exec -- make all

EXPOSE 8080
CMD ["bash", "-lc", "opam exec -- make run.local"]