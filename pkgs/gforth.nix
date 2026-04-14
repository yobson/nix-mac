{ stdenv, fetchgit, pkgs }:

stdenv.mkDerivation {
  pname = "gforth";
  version = "0.7.3";
  src = fetchgit {
    url = "https://git.savannah.gnu.org/git/gforth.git";
    sha256 = "sha256-Nb5CB2k7gfG3sT+zfHGmj9G/CGccIvSIKcOuP7Altn0=";
    rev = "b4f478ed6fb82b90e03910b31a7a78de1663bf28";
  };

  nativeBuildInputs = [ pkgs.m4 pkgs.autoconf pkgs.automake pkgs.libtool ];

  preConfigure = ''
    autoreconf -i
  '';
}
