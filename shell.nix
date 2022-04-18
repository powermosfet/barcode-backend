{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/tarball/21.11") {}
}:

pkgs.mkShell {
  nativeBuildInputs = [
    pkgs.cabal2nix
    pkgs.ghc
    pkgs.cabal-install
  ];

  shellHook = ''
    LOCALE_ARCHIVE="$(nix-build --no-out-link '<nixpkgs>' -A glibcLocales)/lib/locale/locale-archive"

    export LANG=en_GB.UTF-8
  '';
}
