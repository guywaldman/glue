#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
output_file="$repo_root/skills/glue/REFERENCE.md"

{
  cat <<'EOF'
<!-- Generated from repo docs by scripts/update_glue_skill_reference.sh. Do not edit manually. -->

# Glue Reference

This file mirrors the Glue documentation in this repository for use by the repo-published Glue skill.

EOF

  docs=(
    "$repo_root/README.md"
    "$repo_root"/docs/[0-9][0-9]-*.mdx
  )

  for doc in "${docs[@]}"; do
    relative_path="${doc#"$repo_root"/}"
    cat <<EOF

---

## Source: $relative_path

EOF
    cat "$doc"
    printf '\n'
  done
} > "$output_file"
