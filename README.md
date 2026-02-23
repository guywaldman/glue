# Glue

Glue is an IDL (Interface Definition Language) and toolchain for modeling data and interfaces, and a unified toolchain for code generation, IDE support, and more.  
Designed to be simple, fast, human-friendly, AI-friendly, and language-agnostic.

> [!IMPORTANT]
>
> The Glue toolchain is in **beta**, use for production with caution and at your own risk. Expect breaking changes and bugs, though we will try to minimize these.
> Feedback and contributions are very welcome!

For an interactive playground, documentation and more details see the [Glue website](https://gluelang.dev).

## AI & LLMs

Glue is designed to be LLM-friendly. [/llms.txt](https://gluelang.dev/llms.txt) includes the entirety of the Glue docs (~4K tokens) — simply provide it to your favorite agentic coding solution.  
Glue's syntax is concise and forgiving, making it easy for LLMs to work with while remaining unambiguous and easy to parse.

## Quickstart

### 1. Install

```shell
# Homebrew
brew install guywaldman/tap/glue

# Linux/macOS
curl -fsSL https://github.com/guywaldman/glue/releases/latest/download/install.sh | bash

# Windows (PowerShell)
iwr -useb https://github.com/guywaldman/glue/releases/latest/download/install.ps1 | iex
```

### 2. Create a Glue file

Define your data models and interfaces using the Glue IDL:

```glue
// models.glue

model Person {
  name: string
  age: int
  residence_end_date?: string // Optional fields are denoted with a `?`
  is_employed: bool = false   // Default values are supported
}

model Building {
  name: string
  apartments: Record<int, Apartment>
  address: Address

  // Nested models are supported
  model Address {
    street: string
    city: string
    country_code: string
    zipcode: string
  }
}

// Endpoints (like OpenAPI) are supported
endpoint "GET /building/{building_id}" GetBuilding {
  responses: {
    200: Building
    4XX: ApiError
  }
}
```

### 3. Generate code

Pick a target (`typescript`, `python`, `rust`, `go`, `openapi`, `jsonschema`, `protobuf`) and generate code:

```shell
glue gen typescript -i models.glue -o ./generated
```

### 4. (Optional) Configure code generation

Use a `.gluerc.yaml` file to customize output:

```yaml
global:
  output_base_dir: "./src/generated"

gen:
  - files: "models/*.glue"
    output: "{file_name}.ts"
    config_overrides:
      typescript:
        zod: true
```

### 5. (Optional) Install the VS Code extension

Install the [Glue VS Code extension](https://marketplace.visualstudio.com/items?itemName=guywaldman.glue) for syntax highlighting, diagnostics, hover definitions, go-to definitions, and more.