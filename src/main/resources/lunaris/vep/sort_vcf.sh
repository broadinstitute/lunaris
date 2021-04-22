#!/bin/bash

input=$1
output=$2

tmp_dir=$(mktemp -d -t sort_vcf_XXXXXXXX)

headers="$tmp_dir/headers"
data="$tmp_dir/data"

grep "^#" "$input" > "$headers"
grep -v "^#" "$input" | sort -k1,1V -k2,2n > "$data"

cat "$headers" "$data" > "$output"

rm -rf "$tmp_dir"