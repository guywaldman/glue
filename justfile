build:
	cd glue && cargo build --workspace --all-features

generate: build
	# TODO: Use release
	cd glue && ./target/debug/cli gen jsonschema -i cli/assets/config_schema.glue -o cli/assets/config_schema.json
	cd glue && ./target/debug/cli gen rust-serde -i cli/assets/config_schema.glue -o cli/src/codegen/config_schema_generated.rs
	cd glue && cargo fmt -- cli/src/codegen/config_schema_generated.rs

check-cli:
	just lint-cli
	just test-cli

test-cli:
	cd glue && cargo test --workspace --all-features

lint-cli:
	cd glue && cargo clippy --workspace --all -D warnings
	cd glue && cargo fmt --all -- --check

fix-cli:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

extension-dev:
	cd extension && npm install && npm run package && code --install-extension glue-0.0.1.vsix