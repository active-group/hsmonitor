{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    inputs@{ self, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import ./nix/pkgs.nix { inherit inputs system; };
        packageName = "hsmonitor";
      in
      {
        packages = {
          ${packageName} = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hsmonitor;
          default = self.packages.${system}.${packageName};
        };

        devShells = {
          default = pkgs.haskellPackages.shellFor {
            packages = p: [ p.hsmonitor ];
            nativeBuildInputs = with pkgs; [
              haskellPackages.fourmolu
              haskellPackages.cabal-install
              haskellPackages.ghc
              haskellPackages.hlint
              haskellPackages.ghcid
              haskellPackages.haskell-language-server
              haskellPackages.cabal-fmt
              riemann_c_client
              netcat-openbsd
              chromium
            ];
          };
        };

      }
    );
}
