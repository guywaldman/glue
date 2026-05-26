---
name: glue
description: "Official Glue IDL guide for agents. Use when designing, writing, reviewing, validating, or generating code from Glue files and .gluerc config for TypeScript, Python, Rust, Go, OpenAPI, JSON Schema, or Protobuf."
license: MIT
metadata:
  author: Guy Waldman
  version: "1.1.5" # x-release-please-version
---

# Glue

Glue is a compact Interface Definition Language (IDL) and toolchain for modeling data structures and API interfaces once, then generating useful artifacts for multiple ecosystems. Use Glue when you need a clear source of truth for models, enums, type aliases, API endpoints, OpenAPI, JSON Schema, Protobuf, or language-native types.

## Canonical References

Prefer current public docs over memorized syntax when exact behavior matters:

- LLM-ready docs: https://gluelang.dev/llms.txt
- Documentation site: https://gluelang.dev/docs
- Quickstart: https://gluelang.dev/docs/quickstart
- Language reference: https://gluelang.dev/docs/language-reference
- CLI reference: https://gluelang.dev/docs/cli-reference
- Configuration: https://gluelang.dev/docs/configuration
- Code generation docs: https://gluelang.dev/docs/codegen-typescript, https://gluelang.dev/docs/codegen-python, https://gluelang.dev/docs/codegen-rust, https://gluelang.dev/docs/codegen-go, https://gluelang.dev/docs/codegen-openapi, https://gluelang.dev/docs/codegen-jsonschema, https://gluelang.dev/docs/codegen-protobuf
- Repository: https://github.com/guywaldman/glue
- Repository docs folder: https://github.com/guywaldman/glue/tree/main/docs
- Source docs files: https://github.com/guywaldman/glue/blob/main/docs/04-language-reference.mdx, https://github.com/guywaldman/glue/blob/main/docs/02-cli-reference.mdx, https://github.com/guywaldman/glue/blob/main/docs/03-configuration.mdx
- Examples: https://github.com/guywaldman/glue/tree/main/examples
- Config schema source: https://github.com/guywaldman/glue/blob/main/glue/assets/config_schema.glue

## Agent Workflow

1. Clarify the domain boundary: identify the nouns, stable identifiers, enum vocabularies, request/response shapes, and API operations.
2. Write the smallest useful `*.glue` file first. Add config only when output paths, generator options, or multiple targets matter.
3. Prefer explicit, reusable named models over deeply inlined anonymous structures. Reuse type aliases for IDs, timestamps, and repeated primitives.
4. Validate with `glue check <file>` when the CLI is available.
5. Generate with `glue gen <target> -i <file> -o <output>`, or use `glue gen` / `glue gen --config .gluerc.yaml` for config-driven generation. You may also use `glue gen <target> -i <file> -o -` to print to stdout when the output is small or for debugging.
6. Only when you absolutely must dive deeper, inspect the AST with `glue ast <file>`.
7. When a user asks for a target-specific artifact, consult the target docs or `https://gluelang.dev/llms.txt` before relying on memory.
8. If the source uses imports, keep imports at the top of the file and resolve relative paths from the importing file.

## Core Syntax

- Comments: `// line comment`; doc comments: `/// documentation`.
- Primitive types: `string`, `int`, `bool`, `any`.
- Arrays: `Type[]`.
- Maps: `Record<KeyType, ValueType>`.
- Optional fields: `field?: Type`.
- Defaults: `field: Type = value`.
- Type aliases: `type UserId = string`.
- Models: `model User { id: UserId }`.
- Enums: `enum Status: "pending" | "paid" | "cancelled"`.
- Imports must appear before declarations:
  - `import * from "common.glue"`
  - `import * as common from "common.glue"`
  - `import { User, Address as PostalAddress } from "domain.glue"`
- Field metadata can use `@field(alias="external_name", example="value")`.
- Endpoints use `endpoint "METHOD /path/{param}" Name { ... }` with optional `body`, `headers`, and `responses`.

Supported generation targets are `typescript`, `python`, `rust`, `go`, `openapi`, `jsonschema`, and `protobuf`.

## Modeling Guidelines

- Model domain concepts, not one target language. Avoid naming or structuring fields around a single framework unless the user explicitly asks.
- Use stable aliases for IDs and repeated scalar concepts: `type UserId = string`, `type IsoTimestamp = string`, `type MoneyCents = int`.
- Use enums for closed vocabularies. Keep enum values meaningful wire values, usually lowercase strings.
- Make optionality intentional. Use `?` for data that may be absent; use defaults for values the producer can safely omit.
- Avoid global namespace pollution and leverage nested models, enums, and aliases to keep related concepts together. For example, if `Order` is the only model that uses `OrderStatus`, nesting `OrderStatus` within `Order` keeps the domain tidy and makes it clear that the enum is specific to orders.
- Keep endpoint payloads named. Prefer `CreateOrderRequest` and `OrderResponse` over anonymous ad hoc endpoint-only models.
- Use `Record<string, T>` for keyed objects and dictionaries; use arrays for ordered lists.
- Keep imports simple and acyclic. Split files by domain only when the resulting boundaries are obvious.
- Document public models and important fields with `///`. Good doc comments improve generated schemas and help downstream agents. Internal comments are supported with `//` and are stripped from generated output, so they are ideal for implementation notes and reminders that should not be exposed downstream.
- Treat Glue as the source of truth. When editing generated code, consider whether the change belongs in the `.glue` model instead.
- Validate early. A small `glue check` pass is cheaper than debugging generated TypeScript, Python, or OpenAPI later.

## Current Limitations

Do not invent unsupported syntax. As of the current docs, notable limitations include:

- No generics such as `model Response<T> { data: T }`.
- No intersection types such as `type A = B & C`.
- Endpoint path/query parameter typing and auth scheme modeling are limited compared with full OpenAPI.

When these areas matter, keep the Glue model simple and use generated OpenAPI or target-language code as the integration point for additional framework-specific detail.

## CLI Patterns

```shell
# Validate one file.
glue check api.glue

# Generate one target from one file.
glue gen typescript -i api.glue -o generated/api.ts
glue gen python -i api.glue -o generated/api.py
glue gen openapi -i api.glue -o openapi.yaml

# Generate from stdin or a URL.
cat api.glue | glue gen jsonschema -o schema.json
glue gen go https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue -o generated/models.go

# Use auto-discovered .gluerc / .gluerc.yaml, or pass one explicitly.
glue gen
glue gen --config .gluerc.yaml

# Override config inline.
glue gen typescript -i api.glue -o generated/api.ts --set "watermark=none"
```

## Config Pattern

Prefer `.gluerc.yaml` when a project generates multiple targets:

```yaml
# yaml-language-server: $schema=https://raw.githubusercontent.com/guywaldman/glue/main/glue/assets/config_schema.json

global:
  output_base_dir: generated
  config:
    watermark: short
    lint_suppressions: true

gen:
  - mode: typescript
    files: "schemas/*.glue"
    output: "ts/{file_name}.{file_ext}"
    config_overrides:
      typescript:
        zod: true

  - mode: python
    files: "schemas/*.glue"
    output: "py/{file_name}.{file_ext}"
    config_overrides:
      python:
        data_model_library: pydantic

  - mode: openapi
    files: "schemas/api.glue"
    output: "openapi/{file_name}.{file_ext}"
```

### Documented Configuration Example

Use this pattern when a project has one or more `.glue` files and wants repeatable generation for several consumers. The config below assumes source files live under `schemas/` and writes all generated files under `generated/`.

```yaml
# .gluerc.yaml
#
# The schema line gives editors autocomplete and validation for Glue config.
# It is optional, but highly recommended for humans and agents.
# yaml-language-server: $schema=https://raw.githubusercontent.com/guywaldman/glue/main/glue/assets/config_schema.json

global:
  # Every gen entry writes relative to this directory unless its output is absolute.
  output_base_dir: generated

  # Defaults applied to every generation entry before per-entry overrides.
  config:
    # Watermark modes: full, short, none.
    watermark: short

    # Useful for generated code that intentionally has names or imports that
    # linters might otherwise complain about.
    lint_suppressions: true

    # TypeScript defaults. Individual entries can override these.
    typescript:
      zod: false

    # Python can generate pydantic, dataclasses, attrs, or msgspec models.
    python:
      data_model_library: pydantic
      base_model: pydantic.BaseModel

    # Go packages usually need a stable package name.
    go:
      package_name: commerce

    # Protobuf package name for generated .proto files.
    protobuf:
      package_name: commerce.v1

gen:
  # Generate TypeScript runtime schemas for every Glue file.
  - mode: typescript
    files: "schemas/*.glue"
    output: "typescript/{file_name}.{file_ext}"
    config_overrides:
      typescript:
        zod: true

  # Generate Python models from the same source files, but choose dataclasses
  # for this target instead of the global pydantic default.
  - mode: python
    files: "schemas/*.glue"
    output: "python/{file_name}.{file_ext}"
    config_overrides:
      python:
        data_model_library: dataclasses

  # Generate a single OpenAPI document from the API-facing Glue file.
  - mode: openapi
    files: "schemas/commerce_api.glue"
    output: "openapi/commerce.{file_ext}"

  # Generate Protobuf messages for model-only schemas.
  - mode: protobuf
    files: "schemas/models.glue"
    output: "protobuf/{file_name}.{file_ext}"
```

Run it with:

```shell
# Auto-discover .gluerc.yaml from the current/input directory.
glue gen

# Or choose a config explicitly.
glue gen --config .gluerc.yaml

# Inline overrides are useful in CI or one-off local generation.
glue gen --config .gluerc.yaml --set "watermark=none"
```

## Comprehensive Example

The following single-file example demonstrates the most important Glue features in a compact domain. It is intentionally heavily documented so agents can copy patterns from it.

```glue
// commerce.glue
//
// Glue files are meant to be the source of truth for data contracts.
// This example models a small commerce API that could generate TypeScript
// types, Python models, Rust structs, Go structs, OpenAPI, JSON Schema, or
// Protobuf from the same IDL.

/// Reusable scalar aliases keep domain intent visible in every model.
/// They currently alias primitive types, but the names carry semantic meaning.
type UserId = string
type OrderId = string
type ProductId = string
type IsoTimestamp = string
type MoneyCents = int

/// String enums are ideal for closed vocabularies and wire-level states.
/// Use values that should appear in JSON/OpenAPI payloads.
enum OrderStatus: "draft" | "submitted" | "paid" | "fulfilled" | "cancelled"
enum CurrencyCode: "USD" | "EUR" | "GBP"

/// A user account in the commerce system.
model User {
  /// Stable unique identifier, reused across endpoints and related models.
  id: UserId

  /// Field metadata can provide generator hints such as aliases and examples.
  /// Here the generated wire/schema name can be `email`.
  @field(alias="email", example="ada@example.com")
  email_address: string

  /// Optional fields use `?`; this may be absent from incoming payloads.
  display_name?: string

  /// Defaults describe safe producer defaults and can be reflected downstream.
  active: bool = true

  /// Arrays use the `Type[]` suffix.
  roles: string[]

  /// Nested models are useful for concepts owned by exactly one parent type.
  profile: Profile

  model Profile {
    created_at: IsoTimestamp
    marketing_opt_in: bool = false
  }
}

/// A product that can be ordered.
model Product {
  id: ProductId
  sku: string
  name: string
  active: bool = true

  /// Records model dictionaries/maps. Prefer string keys for JSON-like maps.
  attributes?: Record<string, string>
}

/// Money is modeled explicitly so amount and currency do not drift apart.
model Money {
  amount_cents: MoneyCents
  currency: CurrencyCode = "USD"
}

/// A single line item within an order.
model OrderItem {
  product_id: ProductId
  quantity: int = 1
  unit_price: Money
}

/// The aggregate order model reused by multiple API responses.
model Order {
  id: OrderId
  user_id: UserId
  status: OrderStatus = "draft"
  items: OrderItem[]
  total: Money
  created_at: IsoTimestamp
  submitted_at?: IsoTimestamp
}

/// Request payload for creating an order.
/// Named request/response models are easier to reuse and generate cleanly.
model CreateOrderRequest {
  user_id: UserId
  items: OrderItem[]
}

/// A standard paginated response shape.
model OrderListResponse {
  orders: Order[]
  next_cursor?: string
}

/// A standard API error shape. Use it consistently in 4XX/5XX responses.
model ApiError {
  code: string
  message: string
  details?: Record<string, any>
}

/// List orders.
/// Endpoint paths can include placeholders such as `{user_id}`.
/// Rich query/path parameter modeling is intentionally limited today, so keep
/// path semantics clear in the endpoint name and docs.
endpoint "GET /users/{user_id}/orders" ListOrders {
  headers: {
    /// Optional request ID for tracing across services.
    "X-Request-ID"?: string
  }

  responses: {
    200: OrderListResponse
    4XX: ApiError
    5XX: ApiError
  }
}

/// Create a new order from a JSON request body.
endpoint "POST /orders" CreateOrder {
  body: CreateOrderRequest

  responses: {
    201: Order
    4XX: ApiError
    5XX: ApiError
  }
}

/// Submit an existing order. Multiple explicit media types can be used when
/// an endpoint supports more than one request representation.
endpoint "POST /orders/{order_id}/submit" SubmitOrder {
  body: {
    "application/json": Order
  }

  responses: {
    200: Order
    4XX: ApiError
    5XX: ApiError
  }
}
```

## Review Checklist

Before handing a Glue file back to a user, check:

- Imports are at the top and paths are correct.
- Every referenced type is declared or imported.
- Optional fields use `?`; defaults use `=`.
- Enums are pipe-separated quoted string values.
- Endpoint responses include success and error cases.
- No unsupported generics, intersections, or invented decorators/config keys appear.
- `glue check` passes when the CLI is available.
- Generated output paths are explicit when the user asked for code generation.
