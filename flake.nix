{
  inputs.nixpkgs.url = "github:nixos/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        jdkToUse = pkgs.jdk17;
        sbtWithJRE = pkgs.sbt.override { jre = jdkToUse; };

        startupHook = ''
          JAVA_HOME="${jdkToUse}"
          yarn install
        '';
      in
      {
        devShells.default = pkgs.mkShell {
          packages = [
            jdkToUse
            sbtWithJRE
            pkgs.nodejs
            pkgs.yarn
            pkgs.nodePackages_latest.http-server
            pkgs.bloop
            pkgs.scala-cli
          ];
          shellHook = startupHook;
        };
      }
    );
}
