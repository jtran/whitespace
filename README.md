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

## Running the Demo

1. Build the WebAssembly module (see above)
2. Run a web server in this repo's root directory.  You need a web server to serve the WebAssembly module.
3. Open `index.html`

## Testing

Run tests:

```shell
cargo test
```
