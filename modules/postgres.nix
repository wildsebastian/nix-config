{ config, lib, pkgs, ... }:

{
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql_12;
  services.postgresql.dataDir = "/Users/sebastian/postgresql";
}
