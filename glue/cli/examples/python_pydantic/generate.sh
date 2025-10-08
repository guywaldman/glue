#!/usr/bin/env sh

set -e

glue gen -c .gluerc.yaml -i models.glue -o src/generated_models.py