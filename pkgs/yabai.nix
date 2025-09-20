{
lib,
pkgs,
}:
let
  inherit (pkgs.stdenv.hostPlatform) system;

  replacement =
    {
      "x86_64-darwin" = "arm64e?";
      "aarch64-darwin" = "x86_64";
    }
    .${system} or (throw "Unsupported system: ${system}");
in
  pkgs.stdenv.mkDerivation {
    pname = "yabai";
    version = "HEAD";
    src = pkgs.fetchFromGitHub {
      owner = "koekeishiya";
      repo = "yabai";
      rev = "ff42ceadc92dfc50df63b73e3e1384b8b4059864";
      sha256 = "sha256-DTwRQRiEJUBAp97XiSy4skZuNkVpE2YMXRazlODXf2A=";
    };
    nativeBuildInputs = with pkgs; [
      xxd
      installShellFiles
    ];
    buildInputs = with pkgs; [
      apple-sdk_15
    ];
    doInstallCheck = true;
    preferLocalBuild = true;

    makeFlags = [
      "all"
    ];
    configurePhase = ''
    sed -i 's/-arch ${replacement}//g' makefile
    '';

    installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp ./bin/yabai $out/bin/yabai
    installManPage ./doc/yabai.1

    runHook postInstall
    '';

    meta = {
      platforms = [
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      license = lib.licenses.mit;

    };
  }
