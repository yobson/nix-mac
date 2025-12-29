{pkgs, lib, config, ...}:
let darwinAlt = l: d: if pkgs.stdenv.isLinux then l else d;
in {

  imports = [ ./modules/emacs.nix ];

  home.packages = with pkgs; [
    audacity
    musescore
    (agda.withPackages (p: [ p.standard-library ]))
    (darwinAlt vlc vlc-bin)
    (darwinAlt libreoffice libreoffice-bin)
  ] ++ lib.optionals stdenv.isLinux [
      pkgs.kicad
      pkgs.freecad
      pkgs.transmission-remote-gtk
    ];

  editors.emacs.enable = true;
}
