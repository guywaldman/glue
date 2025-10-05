build:
	cd glue && cargo build --workspace --all-features

generate: build
	#!/usr/bin/env bash

	set -e

	cd glue
	# TODO: Use release
	# ./target/debug/cli gen jsonschema -i assets/config_schema.glue -o assets/config_schema.json
	# ./target/debug/cli gen rust-serde -i assets/config_schema.glue -o cli/src/codegen/config_schema_generated.rs
	# cargo fmt -- cli/src/codegen/config_schema_generated.rs

check-cli:
	just lint-cli
	just test-cli

test-cli:
	cd glue && cargo test --workspace --all-features
	cd glue && cargo test --test e2e_tests -- --test-threads=1

lint-cli:
	cd glue && cargo clippy --workspace --all -D warnings
	cd glue && cargo fmt --all -- --check

fix-cli:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

extension-dev:
	cd extension && npm install && npm run package && code --install-extension glue-0.0.1.vsix