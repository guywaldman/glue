# Glue

Glue is an IDL (Interface Definition Language) and toolchain for modeling data and interfaces, and generating them using a unified toolchain.

> [!IMPORTANT]
>
> The Glue toolchain is in **beta**, use for production with caution and at your own risk. Expect breaking changes and bugs, though we will try to minimize these.
> Feedback and contributions are very welcome!

## Installation

For macOS/Linux (ARM or x86):

```shell
brew install guywaldman/tap/glue
# ...or:
curl -fsSL https://github.com/guywaldman/glue/releases/latest/download/install.sh | bash
```

For Windows (ARM or x86):

```shell
iwr -useb https://github.com/guywaldman/glue/releases/latest/download/install.ps1 | iex
```

## VS Code Extension

Install the VS Code extension: https://marketplace.visualstudio.com/items?itemName=guywaldman.glue

Source lives in [extension](extension) with syntax highlighting and language server features.
