{ mkDerivation, ansigraph, async, attoparsec, base
, base16-bytestring, binary, brick, bytestring, config-ini
, containers, data-default, data-prometheus, hspec, http-client
, microlens, microlens-th, mtl, pretty-relative-time, regex-posix
, safe-exceptions, stdenv, stm, template-haskell, text, time, vty
}:
mkDerivation {
  pname = "clusterview";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async attoparsec base binary bytestring config-ini containers
    data-default data-prometheus http-client mtl safe-exceptions stm
    text time
  ];
  executableHaskellDepends = [
    ansigraph async base brick bytestring containers data-default
    data-prometheus microlens microlens-th mtl pretty-relative-time
    regex-posix stm template-haskell time vty
  ];
  testHaskellDepends = [ base base16-bytestring hspec ];
  homepage = "https://github.com/vpsfreecz/clusterview";
  description = "Cluster vision";
  license = stdenv.lib.licenses.bsd3;
}
