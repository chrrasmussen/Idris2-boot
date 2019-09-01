#!/usr/bin/env bash

idris2_exec=${1:-"../idris2"}

$idris2_exec --cg javascript -o foo 1_Hello.idr && time node foo.js
$idris2_exec --cg javascript -o foo 2_Loop.idr && time node foo.js
$idris2_exec --cg javascript -o foo 3_Sort.idr && time node foo.js
$idris2_exec --cg javascript -o foo 4_Pythag.idr && time node foo.js
# $idris2_exec --cg javascript -o foo 5_Fork.idr && time node foo.js
