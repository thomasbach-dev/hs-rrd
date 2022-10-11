{ mkDerivation, base, lib, rrdtool, sydtest, sydtest-discover }:
mkDerivation {
  pname = "hs-rrd";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  libraryPkgconfigDepends = [ rrdtool ];
  testHaskellDepends = [ base ];
  testToolDepends = [  ];
  description = "Haskell bindings to RRDTool";
  license = lib.licenses.bsd3;
}
