#!/usr/bin/env sh

set -e

glue gen rust-serde -c .gluerc.yaml -i models.glue -o src/generated_models.rs