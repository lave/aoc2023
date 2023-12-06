#!/bin/sh

SRC_FILE=${1:?OCaml source file must be specified as the first argument}
BIN_FILE=${SRC_FILE%%.ml}
INPUT_FILE=${2:-${SRC_FILE%%.ml}.input}

ocamlfind ocamlopt -w -5 -w -8 -w -24 -linkpkg -package str -o "$BIN_FILE" Common.ml "$SRC_FILE" \
    && "./$BIN_FILE" "$INPUT_FILE"
