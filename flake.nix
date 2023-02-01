{
  description = "Kayboard layout diagram generation";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          swoop = pkgs.callPackage ./swoop.nix {};
        in
        rec {
          packages.swoop = swoop;
          defaultPackage = packages.swoop;
          devShell = pkgs.callPackage ./shell.nix {};
        }
      );
}
