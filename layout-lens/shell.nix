{ pkgs ? import <nixpkgs>, ... }:
with pkgs;
mkShell {
  buildInputs =  [
    nodePackages_latest.typescript
    nodePackages_latest.ts-node
    nodejs
    purs
    spago-unstable
    purs-tidy-bin.purs-tidy-0_10_0
    purs-backend-es
    purescript-language-server
  ];
}
