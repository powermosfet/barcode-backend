{
  description = "Barcode backend";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        barcode-backend = pkgs.haskellPackages.callPackage ./barcode-backend.nix { };
        localeArchive =
          if pkgs ? glibcLocales then
            "${pkgs.glibcLocales}/lib/locale/locale-archive"
          else
            null;
      in
      {
        packages = {
          inherit barcode-backend;
          default = barcode-backend;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = barcode-backend;
          exePath = "/bin/barcode-backend";
        };

        checks.default = barcode-backend;

        devShells.default = pkgs.haskellPackages.shellFor {
          packages = _: [ barcode-backend ];
          nativeBuildInputs = [
            pkgs.cabal2nix
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hpack
          ];

          shellHook = ''
            ${pkgs.lib.optionalString (localeArchive != null) ''
              export LOCALE_ARCHIVE="${localeArchive}"
              export LANG=en_GB.UTF-8
            ''}
          '';
        };

        formatter = pkgs.nixfmt-rfc-style;
      }
    );
}

