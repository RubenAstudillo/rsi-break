{ mkDerivation, async, base, config-ini, lens, lib, monomer
, process, text, text-show, time, xdg-basedir
}:
mkDerivation {
  pname = "rsi-break";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async base config-ini lens monomer process text text-show time
    xdg-basedir
  ];
  executableHaskellDepends = [ base monomer process ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd2;
}
