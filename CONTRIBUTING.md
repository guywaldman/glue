# Contributing to Glue

Thanks for contributing. If it is a non-trivial change, be sure to open an issue before submitting a PR so we can discuss scope and direction.  
Glue is in early beta, so for areas like the language, parser, semantic analyzer or codegen - please open an issue first to discuss.

## Prerequisites

- Rust 1.70+ (see https://rustup.rs/) - `curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh` on Linux/macOS
- Node.js 22+ (for the VS Code extension) - `brew install node` on macOS
- [just](https://github.com/casey/just) (for task management) - `brew install just` on macOS
- uv, protoc, protobuf-compiler (for E2E tests) - `brew install uv protobuf` on macOS

## Repo layout

- glue/ contains the Rust workspace (CLI, compiler, codegen, LSP, wasm)
- extension/ contains the VS Code extension
- test-fixtures/ contains Glue samples used by tests

## Development

Consult the [justfile](justfile) for available tasks which should be self-explanatory.  


## AI usage

Use AI and LLMs responsibly. Do not contribute any code that you either didn't write or that you don't fully understand.  
Always review and test AI-generated code before committing.

## Releases

Releases are managed by [release-please](https://github.com/googleapis/release-please).  
When changes land on `main`, it opens a release PR that bumps versions across Rust crates and the VS Code extension.  
Merging that PR creates a `v*` tag, which triggers the existing CI release workflow.