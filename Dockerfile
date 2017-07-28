FROM ocaml/opam
RUN git clone https://github.com/BLACS/API.git
RUN opam pin add -n blacsapi API/ocaml_api
RUN sudo apt-get update && opam update && opam depext -i -y ocurl ounit ppx_deriving_yojson blacsapi
COPY . evaluator
RUN sudo chown -R opam:opam evaluator/
WORKDIR evaluator
RUN eval `opam config env` && make
ENTRYPOINT ["./evaluator"]