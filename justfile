build:
	cd glue && cargo build --workspace --all-features

generate: build
	#!/usr/bin/env bash

	set -e

	cd glue
	cargo run --bin glue -- gen json-schema -i assets/config_schema.glue -o assets/config_schema.json
	cargo run --bin glue -- gen rust-serde -i assets/config_schema.glue -o cli/src/codegen/config_schema_generated.rs
	cargo fmt -- cli/src/codegen/config_schema_generated.rs

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
	cd extension && npm install && npm run package && code --install-extension glue-*.vsix

extension-publish:
	cd extension && npm install && npm test && npm run publish