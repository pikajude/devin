{ mkDerivation, ansi-wl-pprint, attoparsec, base, bytestring
, containers, exceptions, hostname, html-entities, irc, ixset-typed
, lens, lifted-async, lifted-base, logging-effect, machines
, monad-control, mtl, network, resourcet, safe-exceptions, stdenv
, stm, text, time, transformers-base, unordered-containers
, utf8-string
}:
mkDerivation {
  pname = "devin";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-wl-pprint attoparsec base bytestring containers exceptions
    hostname html-entities irc ixset-typed lens lifted-async
    lifted-base logging-effect machines monad-control mtl network
    resourcet safe-exceptions stm text time transformers-base
    unordered-containers utf8-string
  ];
  homepage = "https://github.com/pikajude/devin";
  description = "What melvin \"What kevin should have been\" should have been";
  license = stdenv.lib.licenses.bsd3;
}
