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

install-cli:
	cd glue && cargo build --release --bin glue
	cd glue && cargo install --path cli --bin glue

test:
	cd glue && cargo test --workspace --all-features

test-e2e:
	cd glue && cargo test e2e -- --nocapture

lint:
	cd glue && cargo clippy --workspace --all
	cd glue && cargo fmt --all -- --check

fix:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

extension-dev:
	cd extension && rm -rf out ./*.vsix && npm install && npm run package && code --install-extension glue-*.vsix

extension-publish:
	cd extension && npm install && npm test && npm run publish

precommit:
	prek run --all-files

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