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
	cd glue && cargo test -- --skip e2e
	cd glue && cargo test --test e2e_tests -- --test-threads=1

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