{ inputs, system }:
let
  overlays = import ./overlays.nix;
  config = {
    allowBroken = false;
  };
in
import inputs.nixpkgs { inherit config overlays system; }
