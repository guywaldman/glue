build:
	cd glue && cargo build --workspace --all-features

generate: build
	# TODO: Use release
	cd glue && ./target/debug/gluegen gen jsonschema -i gluegen/assets/config_schema.glue -o gluegen/assets/config_schema.json
	cd glue && ./target/debug/gluegen gen rust-serde -i gluegen/assets/config_schema.glue -o gluegen/src/codegen/config_schema_generated.rs