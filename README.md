# Glue

Glue is a domain-specific language and toolchain for modeling data and generating artifacts like API specs and language-specific models.

> [!IMPORTANT]
>
> The Glue toolchain is in **beta**, use for production with caution and at your own risk. Expect breaking changes and bugs, though we will try to minimize these.
> Feedback and contributions are very welcome!

## Highlights

- Glue DSL for defining models and API endpoints.
- Codegen targets for JSON Schema, OpenAPI, Rust, TypeScript, Python, and more.
- CLI and VS Code extension support.

## Installation

### Quick Install (Linux/macOS)

Install the latest version of the CLI:

```bash
curl -fsSL https://raw.githubusercontent.com/guywaldman/glue/main/install.sh | bash
```

### Build from Source

Requires [Rust](https://rustup.rs/) 1.70 or later:

```bash
git clone https://github.com/guywaldman/glue.git
cd glue
just build
```

## Project Goals

1. Tool suite for generating data models and transformations between them
1. Tool suite for generating API definitions (e.g., OpenAPI) 
1. Simple and intuitive DSL (similar to [protobuf](https://protobuf.dev/)) to generate data models, without assumption of a binary format

## Glue vs. Protobuf

...

## Glue vs. OpenAPI

...

## DSL

### Models

```typescript
model User {
  /// A unique identifier for the user (UUID).
  id: string
  /// The user's email address.
  email: string
  /// The user's age.
  age: int
}
```

### REST API endpoints

```typescript
/// Retrieves a user by its ID.
GET /users/{id}
/// The ID of the user to retrieve.
@path_param id: string
/// Whether to include the user's posts.
@query_param includePosts: bool = false
@header X-Request-ID: string
@response 200: Response200
@response 404: Response404
```

## CLI Quickstart

Install the CLI:

```bash
just install-cli
```

Generate JSON Schema from a Glue file:

```bash
just gen-jsonschema ./path/to/schema.glue ./schema.json
```

Generate OpenAPI:

```bash
just gen-openapi ./path/to/api.glue ./openapi.json
```

Run `glue --help` to see all commands.

## VS Code Extension

Install the VS Code extension: https://marketplace.visualstudio.com/items?itemName=guywaldman.glue

Source lives in [extension](extension) with syntax highlighting and language server features.
