#!/usr/bin/env bash

idris2_exec=${1:-"../idris2"}

$idris2_exec --cg swift -o foo 1_Hello.idr && time ./foo
$idris2_exec --cg swift -o foo 2_Loop.idr && time ./foo
$idris2_exec --cg swift -o foo 3_Sort.idr && time ./foo
$idris2_exec --cg swift -o foo 4_Pythag.idr && time ./foo
# $idris2_exec --cg swift -o foo 5_Fork.idr && time ./foo
