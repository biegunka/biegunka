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
  src = builtins.filterSource (_: type: type != "unknown") ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    acid-state aeson async base bytestring command-qq conduit
    conduit-extra containers cryptohash data-default-class directory
    directory-layout exceptions filepath free hspec HStringTemplate
    lens meep mtl optparse-applicative process resourcet safecopy
    semigroups stm template-haskell temporary text transformers unix
  ];
  testDepends = [
    base containers data-default-class directory directory-layout
    filepath free hspec hspec-expectations-lens lens
    optparse-applicative semigroups temporary text transformers unix
  ];
  homepage = "http://biegunka.budueba.com/";
  description = "Configuration development";
  license = stdenv.lib.licenses.mit;
}
