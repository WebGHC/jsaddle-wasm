# jsaddle-wasm
jsaddle interface for WebGHC

## Build

```
  $ nix-shell # Specify wasm-cross path
  $ cabal new-configure -w wasm32-unknown-unknown-wasm-ghc --with-hc-pkg=wasm32-unknown-unknown-wasm-ghc-pkg
  $ cabal new-build
```
