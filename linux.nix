{pkgs, username, ...}:

{
  home.packages = with pkgs; [
  (writeShellScriptBin "nix-rebuild" ''
    home-manager switch --flake /home/${username}/.config/home-manager#$(hostname)
  '')
    racket
    curl
  ];
}
