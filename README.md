# Glue

A domain-specific language and translator toolchain for modelling data.

> [!NOTE]
>
> The project is under heavy development, and should not be used for production.

## Project Goals

1. Simple solution for generating data models and transformations between them — nothing more, nothing less
1. Simple DSL (similar to [protobuf](https://protobuf.dev/)) to generate data models, without assumption of a binary format

## Components

- `gluelang` - DSL (domain-specific language) for defining data models (similar to [protobuf](https://protobuf.dev/)) 
- `gluecli` - CLI for code generation (e.g., generate TypeScript definitions from a Glue file, generate Markdown tables to describe a Glue file)