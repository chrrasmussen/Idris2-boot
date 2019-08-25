#!/usr/bin/env bash

idris2_exec=${1:-"../idris2"}

echo -n ":c foo main" | $idris2_exec 1_Hello.idr
time ./foo.so

echo -n ":c foo main" | $idris2_exec 2_Loop.idr
time ./foo.so

echo -n ":c foo main" | $idris2_exec 3_Sort.idr
time ./foo.so

echo -n ":c foo main" | $idris2_exec 4_Pythag.idr
time ./foo.so

# echo -n ":c foo main" | $idris2_exec 5_Fork.idr
# time ./foo.so
