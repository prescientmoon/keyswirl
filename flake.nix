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
