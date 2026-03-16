{ inputs, ... }:
let
  customHaskellPackages = self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides =
        hself: hsuper:
        with (super.haskell.lib);
        let
          hsmonitor-src = self.nix-gitignore.gitignoreSource [
            "*.git"
            "dist"
            "dist-newstyle"
          ] ../.;
          hsmonitor = justStaticExecutables (dontHaddock (hself.callCabal2nix "hsmonitor" hsmonitor-src { }));
        in
        {
          # We add ourselves to the set of haskellPackages.
          inherit hsmonitor;
        };
    };
    riemann-client = import ./riemann-client {
      lib = super.lib;
      python3 = super.python3;
      src = inputs.python-riemann-clientSrc;
    };
  };
in
customHaskellPackages
