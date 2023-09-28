{ mkDerivation, async, base, config-ini, directory, filepath, lens
, lib, monomer, process, text, text-show, time
}:
mkDerivation {
  pname = "rsi-break";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    async base config-ini directory filepath lens monomer process text
    text-show time
  ];
  executableHaskellDepends = [ base monomer process ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd2;
}
