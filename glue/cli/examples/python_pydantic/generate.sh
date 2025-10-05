#!/usr/bin/env sh

set -euo pipefail

glue gen py-pydantic -c .gluerc.yaml -i models.glue -o src/generated_models.py