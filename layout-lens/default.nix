{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "layout-lens";

  src = ./src;

  buildInputs = with pkgs; with nodePackages_latest; [
    typescript
    esbuild
    nodejs
  ];

  buildPhase = ''
    esbuild $src/index.ts --bundle --outfile=./out.js --platform=node
  '';

  installPhase = ''
    mkdir $out/bin -p
    cp -rv out.js $out/bin/layout-lens
  '';
}
