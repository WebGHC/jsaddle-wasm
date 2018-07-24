{  } :
let
  initialNixpkgs = import <nixpkgs> {};

  wasm-cross = import (initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "WebGHC";
      repo = "wasm-cross";
      rev = "d9470389fa3b68e3af266f69a485217f64107bf7";
      sha256 = "03zyh786sx33p9prz7b3cya0qhadvvywjyni78ff89ff2bgd9pp2";
      fetchSubmodules = true;
    }) {};

  # or a local path
  # wasm-cross = import /home/divam/nobup/wasm/wasm-cross {};

  nixpkgs = wasm-cross.nixpkgsWasm;
  inherit (nixpkgs.haskell.lib) doJailbreak;

  compiler = nixpkgs.haskell.packages.ghcHEAD;
  pkg = compiler.developPackage {
    root = ./.;
    source-overrides = {
      contravariant = "1.5";
      tagged = "0.8.6";
      # lens = "4.17";
    };
    overrides = self: super: {
      lens = doJailbreak super.lens;
      unliftio-core = doJailbreak super.unliftio-core;
    };
  };
  in pkg
