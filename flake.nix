{
  description = "Colubridae programming language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        haskell-packages = nixpkgs.legacyPackages.${system}.haskell.packages;
        ghcVersion = "ghc912";
        pkgs = import nixpkgs { inherit system; };
      in {
        packages = {
          default = haskell-packages.${ghcVersion}.developPackage {
            root = ./.;
            withHoogle = true;
          };
        };
        devShells = {
          default = pkgs.mkShell {
            nativeBuildInputs = with pkgs; [
              haskell.compiler.ghc912
              (haskell-language-server.override {
                supportedGhcVersions = [ "912" ];
              })
              lldb
              llvmPackages_latest.llvm
              clang
              zlib
              gnumake
              gdb
            ];
          }; };
      });
}
