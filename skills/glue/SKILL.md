---
name: glue
description: Official Glue IDL guide for agents. Use when writing, explaining, validating, configuring, or generating code from Glue files into TypeScript, Python, Rust, Go, OpenAPI, JSON Schema, or Protobuf.
license: MIT
---

# Glue

Use this skill for Glue, a compact IDL for defining models, enums, imports, endpoints, and code generation config.

## Workflow

1. Start with the smallest valid `.glue` file or `.gluerc` config that satisfies the request.
2. Validate Glue source with `glue check <file>` when a CLI is available.
3. Generate code with `glue gen <target> -i <file> -o <output>`, or use `glue gen`/`glue gen --config <path>` for config-driven generation.
4. When exact syntax, config fields, or target behavior matters, read `REFERENCE.md` if present. Published skill releases include it as a generated mirror of this repository's docs.
5. If `REFERENCE.md` is not present or may be stale, fetch `https://gluelang.dev/llms.txt` as the canonical current docs.

## Guardrails

- Stay within documented Glue features; do not invent language constructs, decorators, config fields, or generator options.
- Prefer `.gluerc.yaml` for config examples. Supported config filenames are `.gluerc`, `.gluerc.yaml`, `.gluerc.yml`, and `.gluerc.json`.
- Keep config examples minimal and include `mode` on each `gen` entry for config-driven generation.
- Mention limitations only when relevant to the user's task, especially no generics, no intersections, and limited endpoint auth/parameter support.
- Supported generation targets: `typescript`, `python`, `rust`, `go`, `openapi`, `jsonschema`, and `protobuf`.

## Commands

- Validate: `glue check person.glue`
- Generate TypeScript: `glue gen typescript -i person.glue -o ./generated/person.ts`
- Generate from config: `glue gen --config .gluerc.yaml`
- Generate OpenAPI: `glue gen openapi -i api.glue -o ./openapi.json`
- Inspect IR: `glue ast person.glue`
