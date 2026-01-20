# sqlc-gen-typescript

A rust port of [https://github.com/sqlc-dev/sqlc-gen-typescript](sqlc-gen-typescript) aiming speed and small size of WASM binary.

> [!CAUTION]
> Right now it's just a prototype wich supports only `bun:sqlite`.

## Building

### Dependencies:

- [https://github.com/sagiegurari/cargo-make](cargo-make)
- [https://github.com/WebAssembly/binaryen](binaryen)

### Release build

- `cargo make`
