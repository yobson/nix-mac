{pkgs, ...}:

{
  home.packages = with pkgs; [
    (writeShellScriptBin "nix-rebuild" ''
       sudo darwin-rebuild switch --flake /Users/${username}/.config/nix
       '')
    iterm2
    net-news-wire
    skimpdf
    unnaturalscrollwheels
    iina
  ];

  xdg.configFile = {
    "scripts/open-term.sh" = {
      source = pkgs.replaceVars ./dotfiles/scripts/open-term.sh {
        iTerm2 = if pkgs.stdenv.isDarwin
          then "${pkgs.iterm2}/Applications/iTerm2.app"
          else "";
      };
      executable = true;
    };
    "scripts/switch.sh" = {
      source = ./dotfiles/scripts/switch.sh;
      executable = true;
    };
  };

}
