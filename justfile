build:
	cd glue && cargo build --workspace --all-features

generate: build
	# TODO: Use release
	cd glue && ./target/debug/gluegen gen jsonschema -i gluegen/assets/config_schema.glue -o gluegen/assets/config_schema.json
	cd glue && ./target/debug/gluegen gen rust-serde -i gluegen/assets/config_schema.glue -o gluegen/src/codegen/config_schema_generated.rs
	cd glue && cargo fmt -- gluegen/src/codegen/config_schema_generated.rs

check-cli:
	just lint-cli
	just test-cli

test-cli:
	cd glue && cargo test --workspace --all-features

lint-cli:
	cd glue && cargo clippy --workspace --all -D warnings
	cd glue && cargo fmt --all -- --check

fmt-cli:
	cd glue && cargo fmt --all
	cd glue && cargo clippy --workspace --all --fix --allow-dirty --allow-staged

extension-dev:
	cd extension && npm install && npm run package && code --install-extension glue-0.0.1.vsix