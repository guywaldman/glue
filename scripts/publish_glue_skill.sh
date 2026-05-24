#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/publish_glue_skill.sh --dry-run
  scripts/publish_glue_skill.sh --tag <tag>

Generates the Glue skill reference, verifies committed skill metadata is clean,
then runs `gh skill publish` according to the GitHub CLI docs.

Environment:
  KEEP_GLUE_SKILL_REFERENCE=1  Keep generated skills/glue/REFERENCE.md after exit.
EOF
}

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
reference_file="$repo_root/skills/glue/REFERENCE.md"
mode=""
tag=""

cleanup_generated_reference() {
  if [[ "${KEEP_GLUE_SKILL_REFERENCE:-0}" != "1" ]]; then
    rm -f "$reference_file"
  fi
}

trap cleanup_generated_reference EXIT

while [[ $# -gt 0 ]]; do
  case "$1" in
    --dry-run)
      mode="dry-run"
      shift
      ;;
    --tag)
      mode="publish"
      tag="${2:-}"
      if [[ -z "$tag" ]]; then
        echo "error: --tag requires a value" >&2
        usage >&2
        exit 2
      fi
      shift 2
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      echo "error: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ -n "$(git -C "$repo_root" status --porcelain -- skills/glue/SKILL.md)" ]]; then
  cat >&2 <<'EOF'
error: skills/glue/SKILL.md has uncommitted changes.

Commit SKILL.md before publishing. REFERENCE.md is generated on the fly and is
not expected to be checked in.
EOF
  git -C "$repo_root" status --short -- skills/glue/SKILL.md >&2
  git -C "$repo_root" diff HEAD -- skills/glue/SKILL.md >&2 || true
  exit 1
fi

if [[ -z "$mode" ]]; then
  echo "error: choose --dry-run or --tag <tag>" >&2
  usage >&2
  exit 2
fi

"$repo_root/scripts/update_glue_skill_reference.sh"

if ! command -v gh >/dev/null 2>&1; then
  echo "error: GitHub CLI (gh) is required. Install the latest version from https://cli.github.com/." >&2
  exit 1
fi

if ! gh skill publish --help >/dev/null 2>&1; then
  cat >&2 <<'EOF'
error: this GitHub CLI version does not include `gh skill publish`.

Upgrade gh to a version that includes the `skill publish` command:
https://cli.github.com/manual/gh_skill_publish
EOF
  gh --version >&2 || true
  exit 1
fi

if [[ "$mode" == "dry-run" ]]; then
  gh skill publish "$repo_root" --dry-run
else
  gh skill publish "$repo_root" --tag "$tag"
fi
