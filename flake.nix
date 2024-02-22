{
  description = "Kayboard layout diagram generation";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system}.extend inputs.purescript-overlay.overlays.default;
          layout-lens = pkgs.callPackage ./layout-lens/default.nix { };
        in
        rec {
          packages.layout-lens = layout-lens;
          defaultPackage = packages.layout-lens;
          devShells.layout-lens = pkgs.callPackage ./layout-lens/shell.nix { };
          devShells.qmk = pkgs.callPackage ./keyboards/qmk/shell.nix { };

          apps.generate-layout-previews = {
            type = "app";
            program = pkgs.lib.getExe (pkgs.writeShellApplication {
              name = "generate-layout-previes";
              runtimeInputs = [ layout-lens ];
              text = builtins.readFile ./layout-lens/scripts/generate-layouts.sh;
            });
          };
        }
      );
}
