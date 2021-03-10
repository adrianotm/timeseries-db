{ mkDerivation, acid-state, aeson, array, attoparsec, base
, base-compat-batteries, blaze-html, blaze-markup, bytestring
, containers, directory, dlist, hashable, http-media, lens, lib
, lucid, mtl, safecopy, scientific, servant, servant-server
, string-conversions, text, time, transformers
, unordered-containers, vector, wai, warp
}:
mkDerivation {
  pname = "server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state aeson array attoparsec base base-compat-batteries
    blaze-html blaze-markup bytestring containers directory dlist
    hashable http-media lens lucid mtl safecopy scientific servant
    servant-server string-conversions text time transformers
    unordered-containers vector wai warp
  ];
  executableHaskellDepends = [
    acid-state aeson array attoparsec base base-compat-batteries
    blaze-html blaze-markup bytestring containers directory dlist
    hashable http-media lens lucid mtl safecopy scientific servant
    servant-server string-conversions text time transformers
    unordered-containers vector wai warp
  ];
  homepage = "https://github.com/githubuser/server#readme";
  license = lib.licenses.bsd3;
}
