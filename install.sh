#!/usr/bin/env bash
set -euo pipefail

# === Config ===
BINARY_NAME="${BINARY_NAME:-gluegen}"  # Default to gluegen, can be set to gluelang
REPO="guywaldman/glue"
INSTALL_DIR="${INSTALL_DIR:-/usr/local/bin}"
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

case "$ARCH" in
    x86_64) ARCH="amd64" ;;
    aarch64 | arm64) ARCH="arm64" ;;
    *) err "Unsupported architecture: $ARCH" ;;
esac

TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"' EXIT

# === Fetch latest version if needed ===
if [ "$VERSION" = "latest" ]; then
    echo "Fetching latest version..."
    VERSION=$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" \
        | grep -o '"tag_name": *"[^"]*"' \
        | head -n1 | cut -d'"' -f4)
    [ -z "$VERSION" ] && err "Could not determine latest version"
fi

echo "Installing $BINARY_NAME $VERSION for $OS/$ARCH ..."

# === Download & Extract ===
TARBALL_URL="https://github.com/$REPO/releases/download/$VERSION/${BINARY_NAME}_${OS}_${ARCH}.tar.gz"
echo "Downloading from $TARBALL_URL ..."
curl -fsSL "$TARBALL_URL" -o "$TMPDIR/$BINARY_NAME.tgz" || err "Failed to download release"

tar -xzf "$TMPDIR/$BINARY_NAME.tgz" -C "$TMPDIR" || err "Failed to extract archive"

# === Install ===
echo "Installing to $INSTALL_DIR/$BINARY_NAME ..."
install -m 0755 "$TMPDIR/$BINARY_NAME" "$INSTALL_DIR/$BINARY_NAME" || err "Failed to install binary (may need sudo)"

echo "✅ Installed $BINARY_NAME to $INSTALL_DIR/$BINARY_NAME"
echo "Run '$BINARY_NAME --help' to get started."
