# whitespace

A parser for a language with significant whitespace.

## Building

Build the CLI:

```shell
cargo build --release
```

Build the WebAssembly module.  Output is `pkg/`.

```shell
wasm-pack build --target web
```

## Testing

Run tests:

```shell
cargo test
```
