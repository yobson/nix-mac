{pkgs, ...}:

imports = [ ./xmonad.nix ];

{
  home.packages = with pkgs; [
    firefox
    alacritty
    julia-mono
    nerd-fonts.fira-code
  ];

  x11.xmonad = {
    enable = true;
    terminal = "${pkgs.alacritty}/bin/alacritty";
  };

  fonts.fontconfig = { 
    enable = true;
    defaultFonts = {
      monospace = [ "FiraCode Nerd Font Mono" "JuliaMono" ];
    };
  };
}
