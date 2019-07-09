#/usr/bin/env bash

trash src/YafflePaths.*
find . -name "*.ibc" | xargs trash
find . -name "*.ttc" | xargs trash
find . -name "*.ttm" | xargs trash
