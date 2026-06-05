{pkgs,...}:
{
  config = {
    environment.systemPackages = with pkgs; [
      (writeShellScriptBin "nix-rebuild" ''
        sudo system-manager switch --flake ~/.config/nix#$(hostname)
      '')
    ];

    nix.settings.experimental-features = "nix-command flakes";

  };
}
