{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdkToUse = pkgs.jdk17;
        sbtWithJRE = pkgs.sbt.override { jre = jdkToUse; };
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            jdkToUse
            sbtWithJRE
            pkgs.nodejs
          ];
        };
      }
    );
}
