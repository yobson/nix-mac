{ stdenv, fetchzip, pkgs }:

stdenv.mkDerivation {
  pname = "gforth";
  version = "0.7.3";
  src = fetchzip {
    url = "https://ftp.gnu.org/gnu/gforth/gforth-0.7.3.tar.gz";
    sha256 = "sha256-qSrCFQbnVKIpHf+FML85J3kjuGawTuIdlT36FMSK9Nk=";
  };
  nativeBuildInputs = [ pkgs.m4 ];
  
}
