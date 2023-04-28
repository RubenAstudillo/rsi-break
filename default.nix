{ mkDerivation, async, base, lens, lib, monomer, SDL, SDL-mixer
, text, text-show, time
}:
mkDerivation {
  pname = "rsi-break";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base lens monomer SDL SDL-mixer text text-show time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = lib.licenses.bsd2;
  mainProgram = "rsi-break";
}
