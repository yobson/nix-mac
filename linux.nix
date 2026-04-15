{pkgs, username, ...}:

{
  home.packages = with pkgs; [
  (writeShellScriptBin "nix-rebuild" ''
    sudo nixos-rebuild switch --flake /home/${username}/.config/nix#$(hostname)
  '')
    curl
  ];
}
