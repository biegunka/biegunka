{ cabal, acidState, aeson, ansiWlPprint, async, commandQq
, dataDefaultClass, directoryLayout, exceptions, filepath, free
, hspec, hspecExpectationsLens, HStringTemplate, lens, meep, mtl
, optparseApplicative, pointed, reflection, safecopy, semigroups
, stm, tagged, taggedTransformer, temporary, terminalSize, text
, transformers, void
}:

cabal.mkDerivation (self: {
  pname = "biegunka";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    acidState aeson ansiWlPprint async commandQq dataDefaultClass
    directoryLayout exceptions filepath free hspec HStringTemplate lens
    meep mtl optparseApplicative pointed reflection safecopy semigroups
    stm tagged taggedTransformer terminalSize text transformers void
  ];
  testDepends = [
    dataDefaultClass directoryLayout filepath free hspec
    hspecExpectationsLens lens optparseApplicative semigroups temporary
    text transformers
  ];
  meta = {
    homepage = "http://biegunka.budueba.com/";
    description = "Configuration development";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
  doCheck = false; # https://github.com/biegunka/biegunka/issues/62
})
