{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.services.hoogle;

  hoogleEnv = pkgs.buildEnv {
    name = "hoogle";
    paths = [ (cfg.haskellPackages.ghcWithHoogle cfg.packages) ];
  };

in
{

  options.services.hoogle = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable the hoogle service.";
    };

    port = mkOption {
      type = types.port;
      default = 8080;
      description = ''
        Port number Hoogle will be listening to.
      '';
    };

    packages = mkOption {
      # type = types.functionTo (types.listOf types.package);
      default = hp: [ ];
      defaultText = literalExpression "hp: []";
      example = literalExpression "hp: with hp; [ text lens ]";
      description = ''
        The Haskell packages to generate documentation for.
        The option value is a function that takes the package set specified in
        the <varname>haskellPackages</varname> option as its sole parameter and
        returns a list of packages.
      '';
    };

    haskellPackages = mkOption {
      description = "Which haskell package set to use.";
      type = types.attrs;
      default = pkgs.haskellPackages;
      defaultText = literalExpression "pkgs.haskellPackages";
    };

    home = mkOption {
      type = types.str;
      description = "Url for hoogle logo";
      default = "https://hoogle.haskell.org";
    };

    host = mkOption {
      type = types.str;
      description = "Set the host to bind on.";
      default = "127.0.0.1";
    };

    exec = mkOption {
      type = types.str;
      default = "hoogle";
      description = "Hoogle command/binary to execute.";
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ hoogleEnv ];
    launchd.user.agents.hoogle = {
      path = [ hoogleEnv ];
      serviceConfig = {
        ProgramArguments = [
          "${hoogleEnv}/bin/hoogle"
          "server"
          "--local"
          "--port"
          "${toString cfg.port}"
          "--home"
          "${cfg.home}"
          "--host"
          "${cfg.host}"
        ];
        KeepAlive = true;
        RunAtLoad = true;
        # RuntimeDirectory = "hoogle";
        # WorkingDirectory = "%t/hoogle";
      };
    };
  };

}
