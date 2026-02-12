#!/usr/bin/env bash
set -euo pipefail

REPO="guywaldman/glue"
INSTALL_DIR="${INSTALL_DIR:-"$HOME/.local/bin"}"
VERSION="${VERSION:-latest}"

# === Helpers ===
err() { echo "Error: $*" >&2; exit 1; }
need_cmd() { command -v "$1" >/dev/null 2>&1 || err "Missing required command: $1"; }

need_cmd curl
need_cmd tar
need_cmd uname
need_cmd mktemp

OS=$(uname -s | tr '[:upper:]' '[:lower:]')
ARCH=$(uname -m)

case "$OS" in
    linux) OS="linux" ;;
    darwin) OS="darwin" ;;
    *) err "Unsupported OS: $OS (use install.ps1 on Windows)" ;;
esac

case "$ARCH" in
    x86_64) ARCH="amd64" ;;
    aarch64 | arm64) ARCH="arm64" ;;
    *) err "Unsupported architecture: $ARCH" ;;
esac

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

if [ "$VERSION" != "latest" ] && [[ "$VERSION" != v* ]]; then
    VERSION="v$VERSION"
fi

ASSET="glue_${OS}_${ARCH}.tar.gz"
if [ "$VERSION" = "latest" ]; then
    TARBALL_URL="https://github.com/$REPO/releases/latest/download/$ASSET"
else
    TARBALL_URL="https://github.com/$REPO/releases/download/$VERSION/$ASSET"
fi

echo "Installing glue ($VERSION) for $OS/$ARCH ..."
echo "Downloading from $TARBALL_URL ..."
curl -fsSL "$TARBALL_URL" -o "$TMPDIR/glue.tgz" || err "Failed to download release"

tar -xzf "$TMPDIR/glue.tgz" -C "$TMPDIR" || err "Failed to extract archive"

[ -f "$TMPDIR/glue" ] || err "Expected 'glue' in archive, but it was not found"

mkdir -p "$INSTALL_DIR" 2>/dev/null || true

DEST="$INSTALL_DIR/glue"
echo "Installing glue to $DEST..."

if command -v install >/dev/null 2>&1; then
    install -m 0755 "$TMPDIR/glue" "$DEST" 2>/dev/null || err "Failed to write to $DEST (choose a writable INSTALL_DIR or use sudo)"
else
    cp "$TMPDIR/glue" "$DEST" 2>/dev/null || err "Failed to write to $DEST (choose a writable INSTALL_DIR or use sudo)"
    chmod 0755 "$DEST" 2>/dev/null || true
fi

echo "Installed glue to $DEST"
echo "Run 'glue --help' to get started."
