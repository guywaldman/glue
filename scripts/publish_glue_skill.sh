#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'EOF'
Usage:
  scripts/publish_glue_skill.sh --check
  scripts/publish_glue_skill.sh --dry-run
  scripts/publish_glue_skill.sh --tag <tag>

Generates the Glue skill reference, validates that this repository exposes the
Glue skill through the skills CLI, and validates/publishes through GitHub
agent skills.

Environment:
  CI=true  Fail if generated skill files are not committed.
  SKILLS_SH_INSTALL_SOURCE=<url-or-repo>  Install from this public source in a
    temporary directory so skills.sh sees the GitHub repo.
EOF
}

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mode="check"
tag=""

run_skills_cli() {
  if command -v pnpm >/dev/null 2>&1; then
    pnpm dlx skills "$@"
  elif command -v npx >/dev/null 2>&1; then
    npx --yes skills "$@"
  else
    cat >&2 <<'EOF'
error: could not find pnpm or npx.

Install pnpm or Node.js, then rerun this script.
EOF
    exit 1
  fi
}

install_public_skill_for_skills_sh() {
  local source="${SKILLS_SH_INSTALL_SOURCE:-}"

  if [[ -z "$source" ]]; then
    return 0
  fi

  local tmp_dir
  tmp_dir="$(mktemp -d)"

  cleanup_public_install() {
    rm -rf "$tmp_dir"
  }

  trap cleanup_public_install RETURN

  echo "Installing Glue skill from public source for skills.sh indexing: $source"
  (
    cd "$tmp_dir"
    run_skills_cli add "$source" --skill glue --agent codex -y --copy --full-depth
  )
}

run_gh_skill_publish() {
  if ! command -v gh >/dev/null 2>&1; then
    cat >&2 <<'EOF'
error: GitHub CLI (gh) is required.

Install GitHub CLI 2.90.0 or later, then rerun this script.
EOF
    exit 1
  fi

  if ! gh skill publish --help >/dev/null 2>&1; then
    cat >&2 <<'EOF'
error: this GitHub CLI version does not include `gh skill publish`.

Upgrade GitHub CLI to 2.90.0 or later.
EOF
    gh --version >&2 || true
    exit 1
  fi

  if [[ "$mode" == "publish" ]]; then
    gh skill publish "$repo_root" --tag "$tag"
  else
    gh skill publish "$repo_root" --dry-run
  fi
}

while [[ $# -gt 0 ]]; do
  case "$1" in
    --check)
      mode="check"
      shift
      ;;
    --dry-run)
      mode="check"
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

if [[ "$mode" != "check" && "$mode" != "publish" ]]; then
  echo "error: unsupported mode: $mode" >&2
  usage >&2
  exit 2
fi

"$repo_root/scripts/update_glue_skill_reference.sh"

if [[ "${CI:-}" == "true" ]]; then
  if ! git -C "$repo_root" diff --exit-code -- skills/glue/SKILL.md skills/glue/REFERENCE.md; then
    cat >&2 <<'EOF'
error: generated skill files are not committed.

Run `just update-skill`, commit the resulting skill files, and rerun CI.
EOF
    exit 1
  fi
fi

if [[ -n "$tag" ]]; then
  echo "Publishing Glue skill for release tag: $tag"
fi

run_skills_cli add "$repo_root" --skill glue --list --full-depth
install_public_skill_for_skills_sh
run_gh_skill_publish
