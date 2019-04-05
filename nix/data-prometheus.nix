{ mkDerivation, attoparsec, base, bytestring, containers, fetchgit
, hspec, lens, raw-strings-qq, stdenv, wreq
}:
mkDerivation {
  pname = "data-prometheus";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/vpsfreecz/data-prometheus";
    sha256 = "0v0cwdikg31vrvs80yfcbn59pwcw5s1xvmg38bgbsh26vwcdl4gs";
    rev = "683c28187b1f0eb68ff6ba7c607790b0597950b3";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers wreq
  ];
  executableHaskellDepends = [
    attoparsec base bytestring lens wreq
  ];
  testHaskellDepends = [
    attoparsec base containers hspec raw-strings-qq
  ];
  homepage = "https://github.com/vpsfreecz/data-prometheus";
  description = "Prometheus metrics data types and parser";
  license = stdenv.lib.licenses.bsd3;
}
