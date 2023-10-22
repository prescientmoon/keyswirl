{
  description = "Kayboard layout diagram generation";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          layout-lens = pkgs.callPackage ./layout-lens/default.nix { };
        in
        rec {
          packages.layout-lens = layout-lens;
          defaultPackage = packages.layout-lens;
          devShells.layout-lens = pkgs.callPackage ./layout-lens/shell.nix { };
          devShells.qmk = pkgs.callPackage ./keyboards/qmk/shell.nix { };
        }
      );
}
