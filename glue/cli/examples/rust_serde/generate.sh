#!/usr/bin/env sh

set -e

SCRIPT_PATH=$(dirname "$0")

cd $SCRIPT_PATH
cargo run --bin glue -- gen -c .gluerc.yaml -i $SCRIPT_PATH/models.glue -o $SCRIPT_PATH/src/generated_models.rs