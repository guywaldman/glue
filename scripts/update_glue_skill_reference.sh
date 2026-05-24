#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
source_url="${GLUE_SKILL_SOURCE_URL:-https://gluelang.dev/llms.txt}"
output_file="$repo_root/skills/glue/REFERENCE.md"
tmp_file="$(mktemp)"

cleanup() {
  rm -f "$tmp_file"
}

trap cleanup EXIT

curl -fsSL "$source_url" > "$tmp_file"

{
  cat <<EOF
<!-- Generated from $source_url by scripts/update_glue_skill_reference.sh. Do not edit manually. -->

# Glue Reference

This file mirrors the public Glue documentation published at $source_url for use by the repo-published Glue skill.

EOF
  cat "$tmp_file"
} > "$output_file"
