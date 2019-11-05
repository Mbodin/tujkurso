#!/usr/bin/env sh
set -e

COLOR="" # "\e[35m"
ROLOC="" # "\e[0m"

TARGET="main"
DEBUG="false"

if [ $DEBUG = "true" ]
then
  DEBUGFLAG=",debug"
else
  DEBUGFLAG=""
fi

echo "${COLOR}Compiling to bytecode as ${TARGET}…${ROLOC}"
ocamlbuild -use-ocamlfind -Is src \
           -pkgs extlib,lwt,lwt_ppx,ppx_deriving,js_of_ocaml,js_of_ocaml-lwt,js_of_ocaml-ppx,js_of_ocaml-ppx.deriving,js_of_ocaml.deriving \
           -use-menhir -menhir "menhir --explain" \
           -tags "optimize(3)${DEBUGFLAG}" \
           ${TARGET}.byte
echo "${COLOR}Done.${ROLOC}"

if [ $DEBUG = "true" ]
then
  DEBUGFLAG="--pretty"
else
  DEBUGFLAG=""
fi

js_of_ocaml ${DEBUGFLAG} ${TARGET}.byte

sed -i "1i/* The source code of this compiled program is available at https://github.com/Mbodin/tujkurso */" ${TARGET}.js
sed -i "1i/* @license magnet:?xt=urn:btih:0b31508aeb0634b347b8270c7bee4d411b5d4109&dn=agpl-3.0.txt AGPL-v3-or-Later */" ${TARGET}.js
echo "/* @license-end */" >> ${TARGET}.js
echo "//# sourceURL=main.js" >> ${TARGET}.js

echo "${COLOR}Done.${ROLOC}"

