{ mkDerivation, aeson, base, either, HDBC, HDBC-postgresql, hpack
, lib, mtl, servant, servant-server, transformers, wai, warp
}:
mkDerivation {
  pname = "barcode-backend";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base either HDBC HDBC-postgresql mtl servant servant-server
    transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/barcode-backend#readme";
  license = lib.licenses.bsd3;
}
