{ self, ... }:
{
  imports = [ ./hsmonitor ];

  nixpkgs.overlays = [ self.overlays.default ];
}
