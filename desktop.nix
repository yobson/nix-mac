{pkgs, lib, config, ...}:
let darwinAlt = l: d: if pkgs.stdenv.isLinux then l else d;
in {

  imports = [ ./modules/emacs ];

  home.packages = with pkgs; [
    audacity
    musescore
    obsidian
    (agda.withPackages (p: [ p.standard-library ]))
    (darwinAlt vlc vlc-bin)
    (darwinAlt libreoffice libreoffice-bin)
  ] ++ lib.optionals stdenv.isLinux [
      kicad
      freecad
      transmission-remote-gtk
      firefox
      cambalache
      blueprint-compiler
    ];

  editors.emacs.enable = true;
}
