{ config, lib, pkgs, ... }:

{
  services.redis.enable = true;
  services.redis.dataDir = "/Users/sebastian/redis";
}
