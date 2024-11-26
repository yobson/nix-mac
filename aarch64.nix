{pkgs, config, ...}:
{
  imports = [./configuration.nix];
  nixpkgs.hostPlatform = "aarch64-darwin";
}
