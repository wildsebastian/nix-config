{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.sketchybar;

  toSketchybarConfig = opts:
    concatStringsSep "\n" (mapAttrsToList
      (p: v: "sketchybar -m config ${p} ${toString v}")
      opts);

  configFile = mkIf (cfg.config != "")
    "${pkgs.writeScript "sketchybarrc" (
      optionalString (cfg.config != "") cfg.config
    )}";
in

{
  options = with types; {
    services.sketchybar.enable = mkOption {
      type = bool;
      default = false;
      description = "Whether to enable the sketchybar sketchybar.";
    };

    services.sketchybar.package = mkOption {
      type = path;
      default = pkgs.sketchybar;
      description = "The sketchybar package to use.";
    };

    services.sketchybar.config = mkOption {
      type = str;
      default = "";
      example = literalExpression ''
        echo "sketchybar config loaded..."
      '';
      description = ''
        Arbitrary configuration to append to the configuration file.
      '';
    };
  };

  config = mkIf (cfg.enable) {
    environment.systemPackages = [ cfg.package ];

    launchd.user.agents.sketchybar = {
      serviceConfig.ProgramArguments = [ "${cfg.package}/bin/sketchybar" ];

      serviceConfig.KeepAlive = true;
      serviceConfig.RunAtLoad = true;
      serviceConfig.EnvironmentVariables = {
        PATH = "${cfg.package}/bin:${config.environment.systemPath}";
      };
    };
  };
}
