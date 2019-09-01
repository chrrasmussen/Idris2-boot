#!/usr/bin/env bash

idris2_exec=${1:-"../idris2"}

$idris2_exec -o foo 1_Hello.idr && time ./foo.so
$idris2_exec -o foo 2_Loop.idr && time ./foo.so
$idris2_exec -o foo 3_Sort.idr && time ./foo.so
$idris2_exec -o foo 4_Pythag.idr && time ./foo.so
# $idris2_exec -o foo 5_Fork.idr && time ./foo.so
