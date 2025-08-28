#!/usr/bin/env bash

files=("src","app","test")

echo "Formatting Haskell files..."

# shellcheck disable=SC2086
for dir in ${files[@]//,/ }; do
  if [ -d "$dir" ]; then
    echo "Formatting files in directory: $dir"

    find "$dir" -name "*.hs" -o -name "*.lhs" | xargs fourmolu --mode inplace

  else
    echo "Directory $dir does not exist. Skipping..."
  fi
done

echo "Haskell files formatted successfully."
