{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.modules.hsmonitor;

  hsmonitor =
    let
      confighs = pkgs.writeText "Config.hs" (builtins.readFile cfg.config);
    in
    pkgs.haskellPackages.hsmonitor.overrideAttrs {
      preBuild = ''
        cp ${confighs} app/Config.hs
      '';
    };
in
{
  options.modules.hsmonitor = {
    enable = lib.mkEnableOption "hsmonitor";
    config = lib.mkOption {
      type = lib.types.path;
      default = null;
    };
    debug = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    extraBins = lib.mkOption {
      default = [ ];
    };
  };
  config = lib.mkIf cfg.enable {
    users = {
      users.monitor = {
        isNormalUser = true;
        extraGroups = [ "monitor" ];
      };
      groups.monitor = { };
    };

    systemd.services.hsmonitor = {
      enable = true;
      description = "HSMonitor Service";

      wantedBy = [ "multi-user.target" ];
      after = [
        "influxdb.service"
        "riemann.service"
      ];

      path = [
        pkgs.riemann-client
      ]
      ++ cfg.extraBins;

      serviceConfig = {
        Type = "simple";
        User = "monitor";
        Group = "monitor";
        Restart = "always";

        ExecStart = "${hsmonitor}/bin/hsmonitor ${if cfg.debug then " --debug" else ""}";
      };
    };
  };
}
