{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    python-riemann-clientSrc = {
      url = "github:lzszt/python-riemann-client";
      flake = false;
    };
  };
  outputs =
    inputs@{
      self,
      flake-utils,
      python-riemann-clientSrc,
      ...
    }:
    (flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import ./nix/pkgs.nix { inherit inputs system; };
      in
      {
        packages = {
          hsmonitor = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hsmonitor;
          riemann-client = pkgs.riemann-client;
          default = self.packages.${system}.hsmonitor;
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
              pkgs.riemann-client
              netcat-openbsd
              chromium
            ];
          };
        };

      }
    ))
    // {
      overlays.default = import ./nix/overlays.nix { inherit inputs; };
    };
}
