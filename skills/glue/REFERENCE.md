<!-- Generated from https://gluelang.dev/llms.txt by scripts/update_glue_skill_reference.sh. Do not edit manually. -->

# Glue Reference

This file mirrors the public Glue documentation published at https://gluelang.dev/llms.txt for use by the repo-published Glue skill.

---
title: Quickstart
---

> IMPORTANT: If you're using an agentic coding solution, you can provide [gluelang.dev/llms.txt](https://gluelang.dev/llms.txt) as a reference, which contains all of these docs in Markdown format.  
> It will be able to figure out how to bootstrap Glue in your project.


# 1. Install Glue

```shell
# Homebrew
brew install guywaldman/tap/glue

# Linux/macOS
curl -fsSL https://github.com/guywaldman/glue/releases/latest/download/install.sh | bash

# Windows (PowerShell)
iwr -useb https://github.com/guywaldman/glue/releases/latest/download/install.ps1 | iex
```

# 2. Create a Glue file

Start by creating a Glue file that defines your data models and intefaces using the [Glue IDL](/docs/language-reference).  
You can also generate code from a URL or from standard input (stdin).  

Example:

```glue
// models.glue

/// Use triple slash for comments that should be included in generated code as docstrings
model Apartment {
  /// The apartment number, e.g. "1A"
  number: int
  // Use double slash for internal comments and not included in generated code
  residents: Person[]
}

model Person {
  name: string
  age: int
  residence_end_date?: string // Optional fields are denoted with a `?`

  is_employed: bool = false // Default values are supported
}

model Building {
  name: string
  apartments: Record<int, Apartment> // Complex types like maps, lists, etc. are supported

  address: Address

  // Nested models are supported
  model Address {
    street: string
    city: string
    country_code: string
    zipcode: string
  }
}

// Glue supports endpoints (like OpenAPI), which you can optionally define with sane defaults.
/// Get building information by the building ID
endpoint "GET /building/{building_id}" GetBuilding {
    responses: {
        200: Building
        4XX: ApiError
        5XX: ApiError
    }
}

model ApiError {
    code: Code
    message: string

    enum Code: "INVALID_REQUEST" | "NOT_FOUND" | "INTERNAL_ERROR"
}
```

Glue also support imports, so you can split your models into multiple files and import them:

```glue
import * from "models/person.glue"
import * as common from "models/common.glue" // Namespaced import
import { Address } from "models/building.glue" // Direct symbol import
```

# 3. Generate code

Pick a target (e.g., `typescript`, `python`, `rust`, `go`, `openapi`, `jsonschema`, `protobuf`) and generate code:

```shell
glue gen typescript -i models.glue -o ./generated

# If you wish to validate the Glue file without generating code, run:
glue check models.glue
```

# 4. Configure code generation (optional)

To support different use-cases, you can configure code generation with a `.gluerc` file. For example:

```yaml
# .gluerc.yaml
global:
  watermark: "none" # Don't generate a watermark
  lint_suppressions: false # Don't generate lint suppression comments in generated code
  output_base_dir: "./src/generated" # Base output directory for all generated code

gen:
  - files: "models/*.glue"
    output: "{file_name}.ts" # Output path template (relative to output_base_dir)
    config_overrides:
      typescript:
        zod: true # Emit Zod types for TypeScript generation
  - files: "schemas/*.glue"
    output: "schemas/{file_name}.py"
    config_overrides:
      python:
        data_model_library: "dataclasses" # Use Python dataclasses instead of the default Pydantic
```

# 5. Install VS Code extension (optional)

For a better development experience, install the [Glue VS Code extension](https://marketplace.visualstudio.com/items?itemName=guywaldman.glue) for syntax highlighting, error diagnostics, hover definitions, go-to definitions and more for Glue files (by default, those ending with `.glue`).
---
title: Overview
---

Glue is a language for modeling data structure and interfaces, with an ecosystem of tooling for code generation, IDE support, and more.  
It is designed from the ground up to be simple, human-friendly, AI-friendly, and language-agnostic.  

For the motivation behind Glue and the challenges it aims to solve, see [this blog post](https://guywaldman.com/posts/introducing-glue) by its creator.  

If you're familiar with OpenAPI, Protobuf, Smithy or even Avro IDL, you can think of Glue as a batteries-included and minimalistic alternative to those, with a focus on ease of use and flexibility.  

Glue's philosophy is:
1. **Easy for common users, extensible for power users**.  
   Glue wants you (or your LLM) to write as little code as possible. There should be sane defaults for everything.
1. **Generic with escape hatches**  
   Code generation is a never ending arms race against the complexity of target languages and frameworks. Glue should be as generic as possible, but provide escape hatches for advanced use cases.  
1. **Just Work™**  
   Glue should be fast and reliable. It should "just work" for the vast majority of use cases, without needing to fight with it. In addition, Glue should provide helpful error messages when it doesn't work, and ideally even suggest fixes (inspired by Rust's compiler). 

# Motivation

Glue empowers you to define your data models and interfaces in a single source of truth, and revolve your business logic around them.  
It alleviates the need for multiple disparate tools for generating code (e.g., web servers/clients, OpenAPI specs, Protobuf schemas) in favor of a single unified toolchain.  

The design goal for a single, simple, fast and reliable toolchain is inspired by projects such as [Rust's Cargo](https://doc.rust-lang.org/cargo/) and [uv](https://docs.astral.sh/uv/).

In fact, [**Glue's configuration is itself written in Glue**](https://github.com/guywaldman/glue/blob/main/glue/assets/config_schema.glue) and is used to generate **Rust code** which defines the models (with serialization/deserialization) as well as a **JSON schema**!

# Quickstart

Refer to the [Quickstart](/docs/quickstart) page for a quick introduction to using Glue, or if you're using an agentic coding solution, provide [gluelang.dev/llms.txt](https://gluelang.dev/llms.txt) as a reference, which contains all of these docs in Markdown format.  

# The Glue toolchain

## Glue IDL

An Interface Definition Language (IDL) for modeling data models and API endpoints in a way that aims to be as simple and generic as possible, while providing adequate "escape hatches" for advanced use cases. For more information, see the [Language Reference](/docs/04-language-reference) page.

An example Glue model definition:

```glue
model User {
    name: string
    age: int
    email?: string
    active: bool = true
}
```

## Glue CLI & code generation

A command-line interface (CLI) tool for working with Glue files, generating code, and more. For more information, see the [CLI Reference](/docs/cli-reference) page.

Example usage:

```shell
glue check user.glue # Lint and validate a Glue file
glue gen typescript -i user.glue -o ./generated # Generate TypeScript code from a Glue file
```

### Installation

Glue supports multiple platforms (Linux, macOS, Windows) and multiple architectures (x86, ARM).  

To install:

```shell
# Homebrew
brew install guywaldman/tap/glue

# Linux/macOS
curl -fsSL https://github.com/guywaldman/glue/releases/latest/download/install.sh | bash

# Windows (PowerShell)
iwr -useb https://github.com/guywaldman/glue/releases/latest/download/install.ps1 | iex
```

## IDE support

Glue has a [VS Code extension](https://marketplace.visualstudio.com/items?itemName=guywaldman.glue) that provides syntax highlighting, error diagnostics, hover definitions, go-to definitions and more for Glue files (by default, those ending with `.glue`).  


### Usage

Glue has a command-line interface (CLI) tool that you can install and use to work with Glue files, generate code, and more.  

Below is a simple Glue model definition for a `User` type, with some fields.

```glue
model User {
    name: string
    age: int
    email?: string
    active: bool = true
}
```

Then use the CLI to validate and generate code:

```shell
glue check user.glue
glue gen typescript -i user.glue -o ./generated
```

Glue currently supports generation for `typescript`, `python`, `rust`, `go`, `protobuf`, `openapi`, and `jsonschema`.
---
title: CLI reference
---

# Available commands

```shell
$ glue --help

Usage: glue <COMMAND>

Commands:
  check  Checks for validity of a Glue file
  gen    Generates code from a Glue file
  ast    Emits Glue IR as JSON
  help   Print this message or the help of the given subcommand(s)

Options:
  -h, --help     Print help
  -V, --version  Print version
```

# Code generation

Glue generates code from Glue files using the `gen` command. The generated code can be used for:
1. Serialization/deserialization
1. Data schemas (e.g. JSON Schema, Protobuf messages)
1. Generating OpenAPI spec from which you can generate API clients or server stubs

Glue currently supports these languages and formats for code generation (click for more details):
1. [OpenAPI](./codegen-openapi)
1. [JSON Schema](./codegen-jsonschema)
1. [TypeScript](./codegen-typescript) (including Zod types)
1. [Python](./codegen-python) (including support for `pydantic`, `dataclasses`, `attrs`, and `msgspec`)
1. [Rust](./codegen-rust)
1. [Go](./codegen-go)
1. [Protobuf](./codegen-protobuf)

To generate code from a Glue file:

```shell
# Generate code from a Glue file
glue gen <language> api.glue -o ./generated
# ...or from a URL:
glue gen python https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue -o ./models.py

# ...or from stdin:
cat api.glue | glue gen <language> -o ./generated

# ...or into stdout:
glue gen <language> -i api.glue > ./generated
```

# Configuration

If you have a `.gluerc` YAML file (or `.gluerc.yaml`, `.gluerc.yml`, `.gluerc.json`) at the same directory level, Glue will automatically use it for configuration. You can also specify a config file with `--config path/to/config`. 
You can also override specific config options inline with `--set key=value` (e.g. `--set watermark=none` to disable the watermark in generated code).

```shell
# Use `.gluerc` in the current directory if it exists
glue gen go https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue
# ...or specify a config file explicitly
glue gen go https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue --config path/to/.gluerc.yaml
# ...or override specific config options inline:
glue gen go https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue --set "watermark=none"
```


For configurations, see [Configuration](/docs/configuration).  

# Static validation

Glue files can be statically validated with the `check` command:

```shell
glue check api.glue
```

## Imports

Glue resolves imports recursively for both `check` and `gen`.

- Local file inputs resolve imports relative to the importing file.
- URL inputs resolve imports relative to the base URL.
- Import cycles are handled safely (already-visited sources are skipped).
- Imports must be declared at the top of the file.

# Advanced usage

## Inspect Glue IR

Glue can emit the intermediate representation (IR) it uses for code generation, which can be useful for debugging and understanding how Glue processes your files:

```shell
glue ast https://raw.githubusercontent.com/guywaldman/glue/refs/heads/main/examples/basic.glue
```
---
title: Quickstart
---

Create a Glue file, validate it, and generate code in a few commands.

## 1) Create a model

Create a file named `person.glue`:

```glue
model Person {
	name: string
	age: int
	address: Address

	model Address {
		street: string
		city: string
		country_code: string
		zipcode: string
	}
}
```

## 2) Validate it

```shell
glue check person.glue
```

If the model is valid, the command exits successfully without errors.

## 3) Generate code

Generate TypeScript code into `./generated`:

```shell
glue gen typescript -i person.glue -o ./generated
```

You can replace `typescript` with any supported target:

- `python`
- `rust`
- `go`
- `protobuf`
- `openapi`
- `jsonschema`

## 4) Use project configuration (optional)

If your project has a `.gluerc` (or `.gluerc.yml`, `.gluerc.yaml`, `.gluerc.json`), run:

```shell
glue gen --config path/to/.gluerc
```

See the [Configuration](./03-configuration) page for full details.

---
title: Configuration
---

Use a `.gluerc` file to set defaults and per-target overrides for code generation.

## Supported config files

Glue auto-detects these files next to your input file:

- `.gluerc` (YAML)
- `.gluerc.yaml`
- `.gluerc.yml`
- `.gluerc.json`

If no config file is found, Glue uses built-in defaults.

To use a specific config file:

```shell
glue gen --config path/to/.gluerc ...
```

## How the config is structured

- `global`: defaults applied to every generation entry.
- `gen`: a list of per-input rules (`files`) with optional output and overrides.

## Example

```yaml
global:
  output_base_dir: ./generated
  config:
    lint_suppressions: true
    watermark: short
    typescript:
      zod: false
    python:
      data_model_library: pydantic
      base_model: pydantic.BaseModel
    rust:
      include_yaml: true
    go:
      package_name: glue
    protobuf:
      package_name: glue

gen:
  - files: "**/*.glue"
    output: "src/types/{file_name}.ts"
    config_overrides:
      typescript:
        zod: true

  - files: "schemas/api/*.glue"
    output: "src/generated/{file_name}.py"
    config_overrides:
      python:
        data_model_library: dataclasses
      lint_suppressions: false

  - files: "models/*.glue"
    output: "proto/{file_name}.proto"
    config_overrides:
      protobuf:
        package_name: myapp.v1
```

## Notes

- `output` supports `{file_name}` and `{file_ext}` placeholders.
- `watermark` supports `full`, `short`, or `none`.
- `python.data_model_library` supports `pydantic`, `dataclasses`, `attrs`, or `msgspec`.---
title: Language reference
---

> NOTE: This page explains the Glue syntax in pseudo EBNF form. It is not a formal specification, but rather a reference guide to the language features and syntax, so that it's easier to grok.
> If you are interested in the exact grammar, Glue uses [pest](https://pest.rs/) and you can check out the Pest file in the Glue codebase.

The Glue IDL is designed to be simple and intuitive, with a syntax that is easy to read and write. Below is a reference of the language features and syntax.

# Primitive types

Glue supports the following primitive types:
- `string`
- `int`
- `any`
- `bool`

# Compound types

- `model` (a structured type with named fields)
- `enum` (enumeration of string values)
- `T[]` (an array/list of type `T`)
- `Record<T, U>` (a map/dictionary type with keys of type `T` and values of type `U`)
- `endpoint` (an API endpoint definition with method, path, parameters, and responses)

# Models

**Models** are the foundation of Glue data models.
They are defined using the `model` keyword, followed by the model name and a block of fields.

```glue
/// Optional public documentation
model <model_name> {
  <field_name>[?]: <field_type> [= <default_value>]
  ...
}
```

For example:

```glue
/// A user of the system
model User {
  name: string
  age?: int // Optional fields are denoted with a `?`
  active: bool = true // Default values are supported
  contact: ContactInfo

  // Models can be nested
  model ContactInfo {
    email: string
    phone?: string
  }
}
```

## Model decorators

Fields can be decorated with the `@field` decorator, which allows you to specify additional metadata for the field that can be used by code generators.

```glue
model User {
  @field(alias="email", example="user@example.com")
  email_address: string
}
```

# Enums

**Enums** are defined using the `enum` keyword, followed by the enum name and pipe-separated primitive string values.

```glue
enum <enum_name>: "<value1>" | "<value2>" | ...
```

For example:

```glue
enum Color: "red" | "green" | "blue"
```

Enums can be nested inside models as well:

```glue
model Product {
  name: string
  category: Category

  enum Category: "electronics" | "clothing" | "books"
}
```

# Imports

Glue supports explicit imports, which must appear at the top of the file (before any `model`, `endpoint`, or `enum` declarations).

```glue
// Import all exported symbols
import * from "models/common.glue"

// Import all symbols under a namespace alias
import * as common from "models/common.glue"

// Import selected symbols (with optional aliasing)
import { User, Address as PostalAddress } from "models/domain.glue"
```

Notes:

- Import sources are string literals.
- Imports are supported for both local files and HTTP(S) URLs (for URL-based inputs).

# Endpoints

**Endpoints** are defined using the `endpoint` keyword, followed by the HTTP method and path, endpoint name, and a block of parameters and responses.
They mostly follow what you expect from the OpenAPI specification, with defaults such that typing out endpoints is as frictionless as possible.

```glue
endpoint "<http_method> <path>" <optional_name> {
  // Optional body (e.g., for POST/PUT/PATCH requests)
  // Flavor A - single type (implicitly for "application/json")
  body: <type>
  // Flavor B - multiple types with explicit MIME types
  body: {
    "application/json": <json_type>
    "application/xml": <xml_type>
    ...
  }

  // Optional schema for respones
  responses: {
    <status_code or 1XX/2XX/3XX/4XX/5XX>: <response_type>
  }

  // Optional schema for headers
  headers: {
    /// An optional unique request identifier for tracing.
    "X-Request-ID"?: string
  }
}
```

For example:

```glue
/// List posts for a user
endpoint "GET /users/{user_id}/posts" ListUserPosts {
    responses: {
        200: PostListResponse
        4XX: ApiError
        5XX: ApiError
    }
}
```

# Current limitations

Below is a non-exhaustive list of features that are commonly requested or expected in an IDL like Glue, but are not currently supported.
If Glue gains some traction, these will be managed as issues and prioritized accordingly, however for now this acts as a reference for common features you may expect to see in Glue but are not yet implemented:
* Endpoints - proper typing of query/path parameters, authentication/authorization schemes, and some other common API features are not yet supported
* Type aliasing (e.g., `type UserID = string`)
* Intersections of types (e.g., `type A = B & C`)
* Generics (e.g., `model Response<T> { data: T }`)


If you would like to see any of these supported in Glue, please open (or upvote) a feature request in the [Glue GitHub repo](https://github.com/guywaldman/glue).

---
title: AI & LLMs
---

To make Glue LLM-friendly, [/llms.txt](/llms.txt) includes the entirety of these docs.  
Simply provide it to your favorite agentic coding solution. It is roughly 4K tokens, and can be reduced if there is popular demand for it.  
We hope that Glue gains adoption and will be included favorably in the training data of future LLMs, so that it can be used out-of-the-box without needing to provide the docs.

Note that Glue is designed to be human-friendly and additionally LLM-friendly, at least to a reasonable extent.  
LLMs can excel at structured formats (such as OpenAPI specs written in JSON/YAML), however they can often make small mistakes due to the verbose and strict nature of those formats.  
Therefore, you will find that Glue's syntax is a bit more concise and forgiving, while still being unambiguous and easy to parse for both humans and machines.  
---
title: OpenAPI
section: Code generation
---

Glue supports `openapi` as a code generation target, allowing you to generate OpenAPI specifications from Glue files. This can be useful for defining RESTful APIs and generating client libraries or server stubs in various programming languages.  
Glue currently supports OpenAPI v3.0, and only JSON output.  

Simply run:

```shell
glue gen openapi -i api.glue -o ./openapi.json
```

## Example

For this Glue spec:

```glue
/// Returns a user by ID.
endpoint "GET /users/{id}" {
	responses: {
		2XX: User
		4XX: ApiError
		5XX: ApiError
	}
}

/// Create a new user.
endpoint "POST /users" {
	body: User

	responses: {
		201: User
		4XX: ApiError
		5XX: ApiError
	}
}

model User {
		id: string
		name: string
		email: string
}

model ApiError {
	error_code: Code
	message?: string

	enum Code: "USER_NOT_FOUND" | "INVALID_REQUEST" | "INTERNAL_ERROR"
}
```

...generating an OpenAPI spec...

```shell
glue gen openapi -i api.glue -o ./openapi.json
```

...will produce:

```json
{
  "openapi": "3.0.0",
  "info": {
    "title": "Generated API",
    "version": "1.0.0"
  },
  "paths": {
    "/users/{id}": {
      "get": {
        "summary": "Returns a user by ID.",
        "description": "Returns a user by ID.",
        "parameters": [
          {
            "name": "id",
            "in": "path",
            "required": true,
            "schema": {
              "type": "string"
            }
          }
        ],
        "responses": {
          "4XX": {
            "description": "4XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/ApiError"
                }
              }
            }
          },
          "2XX": {
            "description": "2XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/User"
                }
              }
            }
          },
          "5XX": {
            "description": "5XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/ApiError"
                }
              }
            }
          },
          "200": {
            "description": "2XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/User"
                }
              }
            }
          }
        }
      }
    },
    "/users": {
      "post": {
        "summary": "Create a new user.",
        "description": "Create a new user.",
        "responses": {
          "4XX": {
            "description": "4XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/ApiError"
                }
              }
            }
          },
          "5XX": {
            "description": "5XX response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/ApiError"
                }
              }
            }
          },
          "201": {
            "description": "201 response",
            "content": {
              "application/json": {
                "schema": {
                  "$ref": "#/components/schemas/User"
                }
              }
            }
          }
        }
      }
    }
  },
  "components": {
    "schemas": {
      "ApiError": {
        "type": "object",
        "required": [
          "error_code"
        ],
        "properties": {
          "error_code": {
            "$ref": "#/components/schemas/Code"
          },
          "message": {
            "type": "string"
          }
        }
      },
      "User": {
        "type": "object",
        "required": [
          "id",
          "name",
          "email"
        ],
        "properties": {
          "name": {
            "type": "string"
          },
          "email": {
            "type": "string"
          },
          "id": {
            "type": "string"
          }
        }
      }
    }
  }
}
```

# Current limitations

* Proper typing of query/path parameters, authentication/authorization schemes are not yet supported---
title: JSON Schema
section: Code generation
---

Somewhat similarly to OpenAPI, Glue offers `jsonschema` as a code generation target, allowing you to generate JSON Schema specifications from Glue files. This can be useful for validating data structures and generating client libraries or server stubs in various programming languages.  
In fact, this is used internally in the Glue codebase for Glue's configuration! See [glue/assets/config_schema.glue](https://github.com/guywaldman/glue/blob/main/glue/assets/config_schema.glue)

Simply run:

```shell
glue gen jsonschema -i config.glue -o ./config.json
```

## Example

For this Glue spec:

```glue
/// Represents a person.
// IMPORTANT: If your file has multiple top-level models, specify `@root` on exactly one model. 
// In this  case, not required.
model Person {
	/// The person's name.
	name: string
	/// The person's age.
	age: int
	/// The person's address.
	address: Address

	/// The person's address.
	model Address {
		/// The street of the address.
		// NOTE: Not case-sensitive.
		street: string
		/// The city of the address.
		city: string
		/// The country code of the address.
		country_code: string
		/// The zipcode of the address.
		zipcode?: string
	}
}

```

...generating a JSON Schema spec...

```shell
glue gen jsonschema -i config.glue -o ./config.json
```

...will produce:

```json
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "title": "Person",
  "type": "object",
  "properties": {
    "address": {
      "$ref": "#/$defs/Person::Address",
      "description": "The person's address."
    },
    "age": {
      "type": "integer",
      "description": "The person's age."
    },
    "name": {
      "type": "string",
      "description": "The person's name."
    }
  },
  "$defs": {
    "Person": {
      "type": "object",
      "properties": {
        "address": {
          "$ref": "#/$defs/Person::Address",
          "description": "The person's address."
        },
        "age": {
          "type": "integer",
          "description": "The person's age."
        },
        "name": {
          "type": "string",
          "description": "The person's name."
        }
      }
    },
    "Person::Address": {
      "type": "object",
      "properties": {
        "city": {
          "type": "string",
          "description": "The city of the address."
        },
        "country_code": {
          "type": "string",
          "description": "The country code of the address."
        },
        "street": {
          "type": "string",
          "description": "The street of the address."
        },
        "zipcode": {
          "type": "string",
          "description": "The zipcode of the address."
        }
      }
    }
  }
}
```---
title: Protobuf
section: Code generation
---

Glue supports `protobuf` as a code generation target, allowing you to generate `.proto` schemas from Glue models and enums.

Simply run:

```shell
glue gen protobuf -i models.glue -o ./models.proto
```

## Configuration

You can configure the emitted protobuf package name:

```yaml
global:
	config:
		protobuf:
			package_name: myapp.v1
```

## Example

For this Glue spec:

```glue
model User {
	id: int
	name: string
	tags: string[]
}

enum Role: "admin" | "user"
```

...generating protobuf...

```shell
glue gen protobuf -i models.glue -o ./models.proto
```

...will produce:

```proto
syntax = "proto3";

package glue;

message User {
		int32 id = 1;
		string name = 2;
		repeated string tags = 3;
}

enum Role {
		admin = 0;
		user = 1;
}
```

## Current limitations

- Optional fields (`?`) are not supported in Protobuf generation.
- `Record<..., ...>` and anonymous model types are not supported.
- Endpoint declarations are ignored by the Protobuf generator.
---
title: Python
section: Code generation
---

Glue supports `python` as a code generation target, allowing you to generate Python models and enums from Glue files.

Simply run:

```shell
glue gen python -i models.glue -o ./models.py
```

## Supported model libraries

Glue can generate Python models using:

- `pydantic` (default)
- `dataclasses`
- `attrs`
- `msgspec`

Configure this in `.gluerc`:

```yaml
global:
	config:
		python:
			data_model_library: pydantic
			base_model: pydantic.BaseModel
```

`base_model` is used only for `pydantic`.

## Example

For this Glue spec:

```glue
model User {
	id: int
	@field(alias="firstName")
	first_name: string
	email?: string
}

enum UserRole: "admin" | "user"
```

...generating Python...

```shell
glue gen python -i models.glue -o ./models.py
```

...will produce code similar to:

```python
from pydantic import BaseModel
from pydantic import Field
from enum import StrEnum
from typing import Any, Annotated, Optional, Union

class User(BaseModel):
		id: Annotated[int, Field()]
		first_name: Annotated[str, Field(alias="firstName")]
		email: Annotated[Optional[str], Field(default=None)]

class UserRole(StrEnum):
		ADMIN = "admin"
		USER = "user"
```

## Notes

- Nested Glue models are emitted as flattened class names (e.g. `Parent_Child`).
- `@field(alias="...")` is applied in all supported Python model libraries.
- Anonymous model type atoms are currently not supported in Python code generation.
---
title: TypeScript
section: Code generation
---

Glue supports `typescript` as a code generation target.

Simply run:

```shell
glue gen typescript -i models.glue -o ./models.ts
```

## Output modes

TypeScript generation supports:

- **Types only** (default): emits `export type ...`
- **Zod mode**: emits Zod schemas and inferred types

Enable Zod mode in `.gluerc`:

```yaml
global:
	config:
		typescript:
			zod: true
```

## Example

For this Glue spec:

```glue
model User {
	name: string
	age: int
	email?: string
}

enum UserRole: "admin" | "user"
```

...generating TypeScript...

```shell
glue gen typescript -i models.glue -o ./models.ts
```

...will produce code similar to:

```ts
export type User = {
	name: string;
	age: number;
	email?: string | null;
};

export type UserRole = "admin" | "user";
```

With `typescript.zod: true`, output is schema-first:

```ts
import { z } from "zod";

export const UserSchema = z.object({
	name: z.string(),
	age: z.number(),
	email: z.string().nullable().optional(),
});
export type User = z.infer<typeof UserSchema>;
```

## Notes

- Nested Glue models are emitted as flattened names (e.g. `Parent_Child`).
- `Record<K, V>` is emitted as `Record<K, V>` (or `z.record(...)` in Zod mode).
- Anonymous model type atoms are currently not supported in TypeScript code generation.
---
title: Go
section: Code generation
---

Glue supports `go` as a code generation target, allowing you to generate Go structs and enums from Glue models.

Simply run:

```shell
glue gen go -i models.glue -o ./models.go
```

## Configuration

You can configure the emitted package name:

```yaml
global:
	config:
		go:
			package_name: myapi
```

## Example

For this Glue spec:

```glue
model User {
	id: string
	@field(alias="first_name")
	firstName: string
	tags?: string[]
}

enum Status: "active" | "inactive"
```

...generating Go...

```shell
glue gen go -i models.glue -o ./models.go
```

...will produce code similar to:

```go
package glue

type User struct {
		Id        string   `json:"id"`
		FirstName string   `json:"first_name"`
		Tags      *[]string `json:"tags,omitempty"`
}

type Status string

const (
		StatusActive   Status = "active"
		StatusInactive Status = "inactive"
)
```

## Notes

- Optional fields are emitted as pointers and include `,omitempty` in JSON tags.
- Unions are emitted as `interface{}`.
- `Record<K, V>` is emitted as `map[K]V`.
- Anonymous model type atoms are currently not supported in Go code generation.
---
title: Rust
section: Code generation
---

Glue supports `rust` as a code generation target, allowing you to generate Rust structs and enums with Serde derives.

Simply run:

```shell
glue gen rust -i models.glue -o ./models.rs
```

## Configuration

Rust generation currently supports:

```yaml
global:
	config:
		rust:
			include_yaml: true
```

When `include_yaml` is enabled, generated models include `from_yaml`/`to_yaml` helper methods.

## Example

For this Glue spec:

```glue
model User {
	id: int
	@field(alias="first_name")
	firstName: string
	email?: string
	metadata: Record<string, any>
}

enum Status: "active" | "inactive"
```

...generating Rust...

```shell
glue gen rust -i models.glue -o ./models.rs
```

...will produce code similar to:

```rust
use std::collections::HashMap;

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, Default)]
pub struct User {
		pub id: i64,
		#[serde(rename = "first_name")]
		pub firstName: String,
		#[serde(skip_serializing_if = "Option::is_none")]
		pub email: Option<String>,
		pub metadata: HashMap<String, serde_json::Value>,
}

#[derive(serde::Serialize, serde::Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Status {
		#[serde(rename = "active")]
		Active,
		#[serde(rename = "inactive")]
		Inactive,
}
```

## Notes

- Optional fields are emitted as `Option<T>`.
- Arrays are emitted as `Vec<T>`.
- `Record<K, V>` is emitted as `HashMap<K, V>`.
- Anonymous model type atoms currently not supported.
