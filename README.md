# sqlc-gen-typescript

A rust port of [sqlc-gen-typescript](https://github.com/sqlc-dev/sqlc-gen-typescript) aiming speed and small size of WASM binary.

> [!CAUTION]
> Right now it's just a prototype wich supports only `bun:sqlite`.

## Building

### Dependencies:

- [cargo-make](https://github.com/sagiegurari/cargo-make)
- [binaryen](https://github.com/WebAssembly/binaryen)

### Release build

- `cargo make`
