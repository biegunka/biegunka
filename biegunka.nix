{ mkDerivation, acid-state, aeson, async, base, bytestring
, command-qq, conduit, conduit-extra, containers, cryptohash
, data-default-class, directory, directory-layout, exceptions
, filepath, free, hspec, hspec-expectations-lens, HStringTemplate
, lens, meep, mtl, optparse-applicative, process, resourcet
, safecopy, semigroups, stdenv, stm, template-haskell, temporary
, text, transformers, unix
}:
mkDerivation {
  pname = "biegunka";
  version = "0.2";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    acid-state async base bytestring command-qq conduit conduit-extra
    containers cryptohash directory directory-layout exceptions
    filepath free hspec HStringTemplate lens meep mtl
    optparse-applicative process resourcet safecopy semigroups stm
    template-haskell temporary text transformers unix
  ];
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra containers
    data-default-class directory filepath lens process resourcet text
    transformers unix
  ];
  testHaskellDepends = [
    base conduit conduit-extra containers data-default-class directory
    directory-layout filepath free hspec hspec-expectations-lens lens
    optparse-applicative process resourcet semigroups temporary text
    transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
