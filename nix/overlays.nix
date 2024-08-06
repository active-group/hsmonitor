{ inputs }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides =
        hself: hsuper:
        let
          dontCheck = super.haskell.lib.dontCheck;
          dontHaddock = super.haskell.lib.dontHaddock;

          hsmonitor-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          hsmonitor = dontHaddock (hself.callCabal2nix "hsmonitor" hsmonitor-src { });
        in
        {
          # We add ourselves to the set of haskellPackages.
          inherit hsmonitor;
        };
    };
  };
in
[ customHaskellPackages ]
