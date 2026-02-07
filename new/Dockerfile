FROM ocaml/opam:debian-12-ocaml-4.14

RUN sudo apt-get update && sudo apt-get install -y \
    m4 \
    pkg-config \
    libsqlite3-dev \
    libssl-dev \
    zlib1g-dev \
    libgmp-dev \
    ca-certificates \
 && sudo rm -rf /var/lib/apt/lists/*
WORKDIR /home/opam/app

COPY --chown=opam:opam h42n42.opam dune-project dune h42n42.conf.in ./
RUN opam update && opam install -y . --deps-only

COPY --chown=opam:opam . .

RUN if [ -d src ]; then \
      cp -r src/* . && rm -rf src ; \
    fi

RUN opam exec -- dune build

EXPOSE 8080

CMD ["opam", "exec", "--", "make", "run"]