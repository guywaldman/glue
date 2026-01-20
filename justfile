assets_dir := "assets"
config_schema := "assets/config_schema.glue"

build:
	cd glue && cargo build --workspace --all-features

build-wasm:
	cd glue/wasm && wasm-pack build --release --target web
	# Then copy the `glue/wasm/pkg` folder to the frontend repo

generate: build
	#!/usr/bin/env bash

	set -e

	cd glue
	cargo run --bin glue -- gen jsonschema -i {{config_schema}} -o {{assets_dir}}/config_schema.json

	config_rust_file="config/src/schema.rs"
	cargo run --bin glue -- gen rust -i {{config_schema}} -o $config_rust_file
	cargo fmt -- $config_rust_file

check-cli:
	just lint-cli
	just test-cli

install-cli:
	cd glue && cargo build --release --bin glue
	cd glue && cargo install --path cli --bin glue

test-cli:
	cd glue && cargo test --workspace --all-features

lint-cli:
	cd glue && cargo clippy --workspace --all -D warnings
	cd glue && cargo fmt --all -- --check

fix-cli:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

extension-dev:
	cd extension && rm -rf out ./*.vsix && npm install && npm run package && code --install-extension glue-*.vsix

extension-publish:
	cd extension && npm install && npm test && npm run publish

# Pre-commit hooks
precommit-install-hooks:
	pre-commit install

precommit:
	pre-commit run --all-files

precommit-staged:
	pre-commit run

# Release - bumps extension and crate versions, tags and pushes
# Requires: cargo install cargo-edit
release bump:
	#!/usr/bin/env bash
	set -e

	# Bump the extension version
	cd extension
	npm version {{bump}} --no-git-tag-version
	NEW_VERSION=$(node -p "require('./package.json').version")
	cd ..

	echo "Bumped extension to version $NEW_VERSION"

	# Bump all Rust crate versions using cargo-edit
	cd glue
	cargo set-version --workspace "$NEW_VERSION"
	cd ..

	echo "Bumped all crates to version $NEW_VERSION"

	# Stage and commit
	git add extension/package.json extension/package-lock.json glue/*/Cargo.toml glue/Cargo.lock
	git commit -m "Release: v$NEW_VERSION"
	git tag "v$NEW_VERSION"

	echo ""
	echo "Created commit and tag v$NEW_VERSION"
	echo ""
	read -p "Push commit and tag to origin? [y/N] " confirm
	if [[ "$confirm" =~ ^[Yy]$ ]]; then
		git push && git push --tags
		echo "Pushed to origin"
	else
		echo "Aborted. You can push manually with: git push && git push --tags"
	fi