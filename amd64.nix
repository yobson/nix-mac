{pkgs, config, ...}:
{
  imports = [./configuration.nix];
  nixpkgs.hostPlatform = "x86_64-darwin";
}
