#!/usr/bin/env bash

idris2_exec=${1:-"../idris2"}

echo -n ":c foo main" | $idris2_exec --cg rust 1_Hello.idr
time ./foo

echo -n ":c foo main" | $idris2_exec --cg rust 2_Loop.idr
time ./foo

echo -n ":c foo main" | $idris2_exec --cg rust 3_Sort.idr
time ./foo

echo -n ":c foo main" | $idris2_exec --cg rust 4_Pythag.idr
time ./foo

echo -n ":c foo main" | $idris2_exec --cg rust 5_Fork.idr
time ./foo
