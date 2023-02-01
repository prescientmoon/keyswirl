{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = with pkgs; with nodePackages_latest; [
    typescript
    nodejs
    ts-node
  ];
}
