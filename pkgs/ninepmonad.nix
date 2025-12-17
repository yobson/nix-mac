{pkgs, mkDerivation, base, fetchgit, lib}:

let
  my-freer-simple = pkgs.haskellPackages.callPackage ./freer-simple.nix {};
in
  mkDerivation {
    pname = "NinePMonad";
    version = "0.1.0.0";
    src = fetchgit {
      url = "https://github.com/yobson/NinePMonad";
      sha256 = "sha256-WtzWhn2rJOuaV2CWbSqicK+GpyUlXypNBcS83esavAg=";
      rev = "e16f18d5506f84bb510bf5ef889e936a650e8ce1";
    };
    isLibrary = true;
    isExecutable = false;
    libraryHaskellDepends = with pkgs.haskellPackages; [
      base bytestring my-freer-simple network mtl network-run 
      stm NineP attoparsec text containers binary filepath unix time
    ];
    doCheck = false;
    license = lib.licenses.lgpl3Plus;

    postConfigure = ''
      substituteInPlace src/Network/NineP/Server.hs --replace 'getEnv "PLAN9"' 'return $ Just "${pkgs.plan9port}/plan9"'
    '';

  }
