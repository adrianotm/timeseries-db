{ mkDerivation, aeson, base, hashable, lens, lib, mtl, safecopy
, scientific, text
}:
mkDerivation {
  pname = "common";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base hashable lens mtl safecopy scientific text
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
