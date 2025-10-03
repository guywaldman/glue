# Glue

A domain-specific language and translator toolchain for modelling data.

> [!NOTE]
>
> The project is under heavy development, and should not be used for production.

## Installation

### Quick Install (Linux/macOS)

Install the latest version of `gluegen`:

```bash
curl -fsSL https://raw.githubusercontent.com/guywaldman/glue/main/install.sh | bash
```

Install the latest version of `gluelang`:

```bash
curl -fsSL https://raw.githubusercontent.com/guywaldman/glue/main/install.sh | BINARY_NAME=gluelang bash
```

### Install Specific Version

```bash
curl -fsSL https://raw.githubusercontent.com/guywaldman/glue/main/install.sh | VERSION=v0.1.0 bash
```

### Install to Custom Location

```bash
curl -fsSL https://raw.githubusercontent.com/guywaldman/glue/main/install.sh | INSTALL_DIR=$HOME/.local/bin bash
```

### Manual Installation

Download the latest release for your platform from the [releases page](https://github.com/guywaldman/glue/releases/latest), extract the archive, and place the binary in your `PATH`.

### Build from Source

Requires [Rust](https://rustup.rs/) 1.70 or later:

```bash
git clone https://github.com/guywaldman/glue.git
cd glue/glue
cargo build --release
# Binaries will be in target/release/gluegen and target/release/gluelang
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
	  /// A unique identifier for the user.
		// INTERNAL NOTE: The ID is a UUID.
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