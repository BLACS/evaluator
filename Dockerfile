FROM ocaml/opam
RUN sudo apt-get update && opam update && opam depext -i -y ocurl ounit ppx_deriving_yojson
COPY . evaluator
RUN sudo chown -R opam:opam evaluator/
WORKDIR evaluator
RUN eval `opam config env` && make
ENTRYPOINT ["./evaluator"]