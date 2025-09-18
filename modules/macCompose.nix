{ lib, pkgs, config, ... }:
with lib;

let cfg = config.macCompose;
in {
  options.macCompose = {
    enable = mkEnableOption "Write Compose File";
  };
}
