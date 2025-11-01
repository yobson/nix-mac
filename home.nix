{ config, lib, pkgs, username, ... }:

{
  imports = [ ./libs/macCompose.nix
              ./modules/emacs/emacs.nix
              ./modules/vim/vim.nix
            ];
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = username;
  home.homeDirectory = if pkgs.stdenv.isDarwin 
    then "/Users/${username}"
    else "/home/${username}";

  home.packages = [
    pkgs.audacity
    pkgs.ispell
    pkgs.hunspell
    pkgs.musescore
    (pkgs.callPackage ./pkgs/gforth.nix {})
    (pkgs.agda.withPackages (p: [ p.standard-library ]))
  ] ++ lib.optionals (pkgs.stdenv.isDarwin) [
    (pkgs.writeShellScriptBin "nix-rebuild" ''
       sudo darwin-rebuild switch --flake /Users/${username}/.config/nix
       '')
    pkgs.iterm2
    pkgs.net-news-wire
    pkgs.skimpdf
    pkgs.unnaturalscrollwheels
    pkgs.iina
    pkgs.vlc-bin
  ] ++ lib.optionals (pkgs.stdenv.isLinux) [
			# pkgs.vlc
      pkgs.racket
			#  pkgs.kicad
			#   pkgs.firefox
   			#   pkgs.transmission-remote-gtk
   			#   pkgs.curl
   			#   pkgs.gcc
   			#   pkgs.gmp
   			#   pkgs.gnumake
   			#   pkgs.ncurses
   			#   pkgs.pkg-config
  ];

  programs.bash = {
    enable = true;
    initExtra = ''
      source ~/.ghcup/env
      export PATH=$PATH:$HOME/.local/bin
      export PLAN9=${pkgs.plan9port}/plan9
      export PATH=$PATH:${pkgs.plan9port}/plan9/bin
      export LANG=en_GB.UTF-8
      export EDITOR=vim
    '';
  };

  programs.gpg = {
    enable = true;
    settings = {
      keyserver = "hkps://keyserver.ubuntu.com";
    };
  };
  services.gpg-agent = {
    enable = true;
    defaultCacheTtl = 600;
    maxCacheTtl = 7200;
    pinentry.package = if pkgs.stdenv.isDarwin 
      then pkgs.pinentry_mac
      else pkgs.pinentry-qt;
    enableScDaemon = false;
  };

  programs.git = {
    enable = true;
    ignores = [
      "*.vscode"
      "dist-newstyle"
    ];
    userEmail = "james@hobson.space";
    userName = "James Hobson";
    signing.signByDefault = true;
    signing.key = "D5E8 7B99 20A0 F392 857E  6212 27B6 62CE FCE9 BE00";
  };

  programs.powerline-go = {
    enable = true;
    settings = {
      jobs = "$(jobs -p | wc -l)";
    };
    pathAliases = {
      "\\~/gits" = "";
      "/Volumes/Projects" = "";
      "\\~/syncthing" = "";
    };
    modules = [
      "nix-shell"
      "cwd"
      "git"
      "jobs"
      "root"
    ];
  };

  editors.emacs.enable = true;
  editors.vim.enable = true;

  xdg.configFile = {
    "dictionaries" = {
      recursive = true;
      source = ./dotfiles/dictionaries;
    };
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

  home.file = {
    ".latexmkrc".source = ./dotfiles/latexmkrc;
    ".ghci".source = ./dotfiles/ghci;
  };

  macCompose = {
    enable = true;
    mapping = {
      ba = "𝕒";
      bb = "𝕓";
      bc = "𝕔";
      bd = "𝕕";
      be = "𝕖";
      bf = "𝕗";
      bg = "𝕘";
      bh = "𝕙";
      bi = "𝕚";
      bj = "𝕛";
      bk = "𝕜";
      bl = "𝕝";
      bm = "𝕞";
      bn = "𝕟";
      bo = "𝕠";
      bp = "𝕡";
      bq = "𝕢";
      br = "𝕣";
      bs = "𝕤";
      bt = "𝕥";
      bu = "𝕦";
      bv = "𝕧";
      bw = "𝕨";
      bx = "𝕩";
      by = "𝕪";
      bz = "𝕫";
      bA = "𝔸";
      bB = "𝔹";
      bC = "ℂ";
      bD = "𝔻";
      bE = "𝔼";
      bF = "𝔽";
      bG = "𝔾";
      bH = "ℍ";
      bI = "𝕀";
      bJ = "𝕁";
      bK = "𝕂";
      bL = "𝕃";
      bM = "𝕄";
      bN = "ℕ";
      bO = "𝕆";
      bP = "ℙ";
      bQ = "ℚ";
      bR = "ℝ";
      bS = "𝕊";
      bT = "𝕋";
      bU = "𝕌";
      bV = "𝕍";
      bW = "𝕎";
      bX = "𝕏";
      bY = "𝕐";
      bZ = "ℤ";
      b0 = "𝟘";
      b1 = "𝟙";
      b2 = "𝟚";
      b3 = "𝟛";
      b4 = "𝟜";
      b5 = "𝟝";
      b6 = "𝟞";
      b7 = "𝟟";
      b8 = "𝟠";
      b9 = "𝟡";

      mca = "𝓪";
      mcb = "𝓫";
      mcc = "𝓬";
      mcd = "𝓭";
      mce = "𝓮";
      mcf = "𝓯";
      mcg = "𝓰";
      mch = "𝓱";
      mci = "𝓲";
      mcj = "𝓳";
      mck = "𝓴";
      mcl = "𝓵";
      mcm = "𝓶";
      mcn = "𝓷";
      mco = "𝓸";
      mcp = "𝓹";
      mcq = "𝓺";
      mcr = "𝓻";
      mcs = "𝓼";
      mct = "𝓽";
      mcu = "𝓾";
      mcv = "𝓿";
      mcw = "𝔀";
      mcx = "𝔁";
      mcy = "𝔂";
      mcz = "𝔃";
      mcA = "𝓐";
      mcB = "𝓑";
      mcC = "𝓒";
      mcD = "𝓓";
      mcE = "𝓔";
      mcF = "𝓕";
      mcG = "𝓖";
      mcH = "𝓗";
      mcI = "𝓘";
      mcJ = "𝓙";
      mcK = "𝓚";
      mcL = "𝓛";
      mcM = "𝓜";
      mcN = "𝓝";
      mcO = "𝓞";
      mcP = "𝓟";
      mcQ = "𝓠";
      mcR = "𝓡";
      mcS = "𝓢";
      mcT = "𝓣";
      mcU = "𝓤";
      mcV = "𝓥";
      mcW = "𝓦";
      mcX = "𝓧";
      mcY = "𝓨";
      mcZ = "𝓩";

      "_0" = "₀";
      "_1" = "₁";
      "_2" = "₂";
      "_3" = "₃";
      "_4" = "₄";
      "_5" = "₅";
      "_6" = "₆";
      "_7" = "₇";
      "_8" = "₈";
      "_9" = "₉";
      "_+" = "₊";
      "_." = "₋";
      "_=" = "₌";
      "_(" = "₍";
      "_)" = "₎";
      "_a" = "ₐ";
      "_e" = "ₑ";
      "_h" = "ₕ";
      "_i" = "ᵢ";
      "_j" = "ⱼ";
      "_k" = "ₖ";
      "_l" = "ₗ";
      "_m" = "ₘ";
      "_n" = "ₙ";
      "_o" = "ₒ";
      "_p" = "ₚ";
      "_r" = "ᵣ";
      "_s" = "ₛ";
      "_t" = "ₜ";
      "_u" = "ᵤ";
      "_x" = "ₓ";
      "^0" = "⁰";
      "^1" = "¹";
      "^2" = "²";
      "^3" = "³";
      "^4" = "⁴";
      "^5" = "⁵";
      "^6" = "⁶";
      "^7" = "⁷";
      "^8" = "⁸";
      "^9" = "⁹";
      "^=" = "⁼";
      "^A" = "ᴬ";
      "^B" = "ᴮ";
      "^C" = "ꟲ";
      "^D" = "ᴰ";
      "^E" = "ᴱ";
      "^F" = "ꟳ";
      "^G" = "ᴳ";
      "^H" = "ᴴ";
      "^I" = "ᴵ";
      "^J" = "ᴶ";
      "^K" = "ᴷ";
      "^L" = "ᴸ";
      "^M" = "ᴹ";
      "^N" = "ᴺ";
      "^O" = "ᴼ";
      "^P" = "ᴾ";
      "^Q" = "ꟴ";
      "^R" = "ᴿ";
      "^T" = "ᵀ";
      "^U" = "ᵁ";
      "^V" = "ⱽ";
      "^W" = "ᵂ";
      "^a" = "ᵃ";
      "^b" = "ᵇ";
      "^c" = "ᶜ";
      "^d" = "ᵈ";
      "^e" = "ᵉ";
      "^f" = "ᶠ";
      "^g" = "ᵍ";
      "^h" = "ʰ";
      "^i" = "ⁱ";
      "^j" = "ʲ";
      "^k" = "ᵏ";
      "^l" = "ˡ";
      "^m" = "ᵐ";
      "^n" = "ⁿ";
      "^o" = "ᵒ";
      "^p" = "ᵖ";
      "^q" = "𐞥";
      "^r" = "ʳ";
      "^s" = "ˢ";
      "^t" = "ᵗ";
      "^u" = "ᵘ";
      "^v" = "ᵛ";
      "^w" = "ʷ";
      "^x" = "ˣ";
      "^y" = "ʸ";
      "^z" = "ᶻ";

      ga = "α";
      gb = "β";
      gc = "χ";
      gd = "δ";
      ge = "ε";
      gf = "φ";
      gg = "γ";
      gh = "η";
      gi = "ι";
      gk = "κ";
      gl = "λ";
      gm = "μ";
      gn = "ν";
      go = "ω";
      gp = "ψ";
      gr = "ρ";
      gs = "σ";
      gt = "τ";
      gu = "υ";
      gx = "ξ";
      gz = "ζ";
      gC = "Χ";
      gD = "Δ";
      gF = "Φ";
      gG = "Γ";
      gL = "Λ";
      gO = "Ω";
      gP = "Ψ";
      gR = "Ρ";
      gS = "Σ";
      gX = "Ξ";
      gZ = "Ζ";

      lt = "⊤";
      lf = "⊥";

      and = "∧";
      assign = "≔";
      circ = "◯";
      Circ = "●";
      comp = "∘";
      div = "÷";
      equiv = "≡";
      exists = "∃";
      forall = "∀";
      fa = "∀";
      Lambda = "Λ";
      lambda = "λ";
      lub = "⊔";
      mapsto = "↦";
      neg = "¬";
      or = "∨";
      phi = "ϕ";
      psi = "ψ";
      pi = "π";
      Pi = "Π";
      prod = "∏";
      qed = "∎";
      star = "★";
      Sigma = "Σ";
      sum = "∑";
      times = "×";
      to = "→";
      vdash = "⊢";
      vDash = "⊨";
      vee = "∨";
      wedge = "∧";

      "[[" = "⟦";
      "]]" = "⟧";
      ">>" = "⟩";
      "<<" = "⟨";
      "<=" = "≤";
      ">=" = "≥";
      ":" = "∶";
      "|->" = "↦";
      "=" = "＝";
      "~-" = "≃";
      "~=" = "≅";
      "~~" = "≈";
      "~>" = "⇝";
      "|-" = "⊢";
      "."  = "∙";
      ";"  = "⨾";
      "|=" = "⊨";
    };
  };

  programs.direnv = {
    enable = true;
    enableBashIntegration = true;
    nix-direnv.enable = true;
  };

  #  programs.vscode = {
  #    enable = true;
  #    package = pkgs.vscodium;
  #    profiles.default.extensions = with pkgs.vscode-extensions; [
  #      vscodevim.vim
  #      haskell.haskell
  #      mkhl.direnv
  #      banacorn.agda-mode
  #    ];
  #  };

  home.stateVersion = "24.05";

  programs.home-manager.enable = true;
}
