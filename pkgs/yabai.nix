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
    version = "7.1.16";
    src = pkgs.fetchFromGitHub {
      owner = "koekeishiya";
      repo = "yabai";
      rev = "9868ae3fc5348d0fcb54ce71fbd6dd39c87cb61d";
      sha256 = "sha256-WXvM0ub4kJ3rKXynTxmr2Mx+LzJOgmm02CcEx2nsy/A=";
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
