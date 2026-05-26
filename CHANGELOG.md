# Changelog

## [1.1.6](https://github.com/guywaldman/glue/compare/v1.1.5...v1.1.6) (2026-05-26)


### Bug Fixes

* **ci:** don't fail the release is the VSC extension was already published ([d55049a](https://github.com/guywaldman/glue/commit/d55049a676ed0170083280587f9deeacbe9b6b28))

## [1.1.5](https://github.com/guywaldman/glue/compare/v1.1.4...v1.1.5) (2026-05-26)


### Bug Fixes

* **ci:** fix version updates by release-please ([6177f11](https://github.com/guywaldman/glue/commit/6177f11d2edb8f28161356c3ee9012c1ae18b17f))
* **ci:** have release-please update lockfiles ([61e0e9d](https://github.com/guywaldman/glue/commit/61e0e9d16d1fcdacd273d63c0b22c95d13a719dc))
* **ci:** make release-please update the version in SKILL.md ([2264867](https://github.com/guywaldman/glue/commit/226486779d0178a269c1b6a92d1f8708be35931e))
* **cli:** fix issue where the CLI did not display the correct version after releases ([e5f22a0](https://github.com/guywaldman/glue/commit/e5f22a03d76cc2eb6c539590ccae6165d90dad96))

## [1.1.4](https://github.com/guywaldman/glue/compare/v1.1.3...v1.1.4) (2026-05-26)


### Bug Fixes

* **ci:** have release-please update lockfiles ([61e0e9d](https://github.com/guywaldman/glue/commit/61e0e9d16d1fcdacd273d63c0b22c95d13a719dc))
* **ci:** make release-please update the version in SKILL.md ([2264867](https://github.com/guywaldman/glue/commit/226486779d0178a269c1b6a92d1f8708be35931e))
* **cli:** fix issue where the CLI did not display the correct version after releases ([e5f22a0](https://github.com/guywaldman/glue/commit/e5f22a03d76cc2eb6c539590ccae6165d90dad96))
* **skill:** shorten and polish the skill & remove skill reference auto-generation ([459fce1](https://github.com/guywaldman/glue/commit/459fce12dac254ae663809a8ed168ef132329615))

## [1.1.3](https://github.com/guywaldman/glue/compare/v1.1.2...v1.1.3) (2026-05-24)


### Bug Fixes

* fix bug where `glue gen` did not overwrite files ([4c3f192](https://github.com/guywaldman/glue/commit/4c3f1923622aa3a9d51e647f7546689020c07730))

## [1.1.2](https://github.com/guywaldman/glue/compare/v1.1.1...v1.1.2) (2026-05-24)


### Bug Fixes

* fix CI errors when testing/publishing extension ([0f792cf](https://github.com/guywaldman/glue/commit/0f792cfe61825b98e481fe47201fb92e827ebb3b))

## [1.1.1](https://github.com/guywaldman/glue/compare/v1.1.0...v1.1.1) (2026-05-24)


### Bug Fixes

* fix bug where watermark generated the wrong path for the config ([e8bad13](https://github.com/guywaldman/glue/commit/e8bad1326dc82978d3a1ed12b28e8364da369792))
* normalize file paths in watermarks to UNIX ([0b05968](https://github.com/guywaldman/glue/commit/0b0596846c9f3121aa6c98a8b4ccda64c12c3553))

## [1.1.0](https://github.com/guywaldman/glue/compare/v1.0.0...v1.1.0) (2026-05-24)


### Features

* introduce preserve flag & avoid mangling upper casing in generated identifiers ([c96eca6](https://github.com/guywaldman/glue/commit/c96eca6a8cacaadb0e20463497c4d54124781947))

## [1.0.0](https://github.com/guywaldman/glue/compare/v0.8.0...v1.0.0) (2026-05-24)


### ⚠ BREAKING CHANGES

* **cli:** support codegen with multiple outputs

### Features

* **cli:** support codegen with multiple outputs ([dd3345a](https://github.com/guywaldman/glue/commit/dd3345ad79a47dc9d0d9f689964b9d6e0382e282))

## [0.8.0](https://github.com/guywaldman/glue/compare/v0.7.1...v0.8.0) (2026-02-27)


### Features

* support type aliases ([#25](https://github.com/guywaldman/glue/issues/25)) ([588d367](https://github.com/guywaldman/glue/commit/588d3672769ef300a7fcd8bc7a39afd2834fd3b9))


### Bug Fixes

* add test coverage for circular imports ([#28](https://github.com/guywaldman/glue/issues/28)) ([b850624](https://github.com/guywaldman/glue/commit/b8506243991fa75af0d5a49e8d44fb9e67e31e36))
* **extension:** support type alias syntax highlighting ([#29](https://github.com/guywaldman/glue/issues/29)) ([c663b0e](https://github.com/guywaldman/glue/commit/c663b0e799eac6d0a2a9ddca4bd0afb310f572cc))
* support type aliases in extension & LSP ([#27](https://github.com/guywaldman/glue/issues/27)) ([95baf48](https://github.com/guywaldman/glue/commit/95baf48794b579a53bd4a8309a74482b2355346c))

## [0.7.1](https://github.com/guywaldman/glue/compare/v0.7.0...v0.7.1) (2026-02-23)


### Bug Fixes

* change logo to PNG in extension README ([ff6c503](https://github.com/guywaldman/glue/commit/ff6c5032b23909a43a1df739e851805475155bdb))

## [0.7.0](https://github.com/guywaldman/glue/compare/v0.6.0...v0.7.0) (2026-02-23)


### Features

* add screenshot to extension README ([2693c47](https://github.com/guywaldman/glue/commit/2693c47001a03d964846f2a4485d1e649adbb1a9))

## [0.6.0](https://github.com/guywaldman/glue/compare/v0.5.0...v0.6.0) (2026-02-22)


### Features

* add support for body schema in endpoints ([bde0025](https://github.com/guywaldman/glue/commit/bde002593228526c188bbba6d7c8ca6c83153a9a))


### Bug Fixes

* **codegen:** format generated Go code ([9f9b09d](https://github.com/guywaldman/glue/commit/9f9b09d73a3a1ccadb642727daeb65592989235f))

## [0.5.0](https://github.com/guywaldman/glue/compare/v0.4.0...v0.5.0) (2026-02-22)


### Features

* **cli:** support inline CLI config overrides ([#18](https://github.com/guywaldman/glue/issues/18)) ([f18b285](https://github.com/guywaldman/glue/commit/f18b28552916016d73f551dc7159fce92f7d2f54))

## [0.4.0](https://github.com/guywaldman/glue/compare/v0.3.1...v0.4.0) (2026-02-21)


### Features

* add VS Code file explorer icons ([#14](https://github.com/guywaldman/glue/issues/14)) ([9c94420](https://github.com/guywaldman/glue/commit/9c944202d8df8e7dc122d4866a0bc6b3660d590a))
* support imports ([#13](https://github.com/guywaldman/glue/issues/13)) ([72833d8](https://github.com/guywaldman/glue/commit/72833d8a1a2f2b483a94158a3ff602d72c155bde))
* support URL inputs ([#16](https://github.com/guywaldman/glue/issues/16)) ([d7dce79](https://github.com/guywaldman/glue/commit/d7dce79265d3a3692ec3ab768dc117d6925e627a))

## [0.3.1](https://github.com/guywaldman/glue/compare/v0.3.0...v0.3.1) (2026-02-19)


### Bug Fixes

* add missing node types in IR and remove false error nodes ([10d9554](https://github.com/guywaldman/glue/commit/10d955489c206ab6395f78521aa32c69d89dd698))

## [0.3.0](https://github.com/guywaldman/glue/compare/v0.2.0...v0.3.0) (2026-02-18)


### Features

* add auto-discovery for .gluerc files ([c004cd0](https://github.com/guywaldman/glue/commit/c004cd008d10f0faada3fb05915b1595a33eed96))
* make package name configurable for Go & Protobuf ([900f756](https://github.com/guywaldman/glue/commit/900f7565e373c091628f704068ebf055e9672bb9))

## [0.2.0](https://github.com/guywaldman/glue/compare/v0.1.1...v0.2.0) (2026-02-18)


### Features

* add intellisense for decorators ([8d0fc49](https://github.com/guywaldman/glue/commit/8d0fc490227ed5731f420b33c1b0b923efb2c414))
* add LSP diags & support WASM codegen with config ([f4195ee](https://github.com/guywaldman/glue/commit/f4195eecd79b2e36c566bb5e51a9340463001b0d))


### Bug Fixes

* add back support for `include_yaml` in Rust codegen config ([fe811af](https://github.com/guywaldman/glue/commit/fe811af681beca5bcbae848f31b3e8339c99e781))
* fix LSP diags with lenient semantic analysis ([e10ffed](https://github.com/guywaldman/glue/commit/e10ffed4ce05d0a539c7528bd9c87db2be1b91ea))

## [0.1.1](https://github.com/guywaldman/glue/compare/v0.1.0...v0.1.1) (2026-02-12)


### Bug Fixes

* add binaries and VSIX to release artifacts ([58f7d28](https://github.com/guywaldman/glue/commit/58f7d28aa99fe873156927e1c960861e1f059aa5))

## [0.1.0](https://github.com/guywaldman/glue/compare/v0.0.11...v0.1.0) (2026-02-12)


### Features

* add install script to releases ([19a284b](https://github.com/guywaldman/glue/commit/19a284b6344e73ec817e0a448ee86f2e410a0cbb))
