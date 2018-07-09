{ compiler, flags ? {}, hsPkgs, pkgconfPkgs, pkgs, system }:
let
    _flags = {} // flags;
    in {
      flags = _flags;
      package = {
        specVersion = "1.10";
        identifier = {
          name = "cardano-sl-block";
          version = "1.3.0";
        };
        license = "MIT";
        copyright = "2017 IOHK";
        maintainer = "hi@serokell.io";
        author = "Serokell";
        homepage = "";
        url = "";
        synopsis = "Cardano SL - block processing";
        description = "Cardano SL - block processing";
        buildType = "Simple";
      };
      components = {
        cardano-sl-block = {
          depends  = [
            hsPkgs.aeson
            hsPkgs.aeson-options
            hsPkgs.base
            hsPkgs.bytestring
            hsPkgs.cardano-sl-binary
            hsPkgs.cardano-sl-core
            hsPkgs.cardano-sl-crypto
            hsPkgs.cardano-sl-db
            hsPkgs.cardano-sl-delegation
            hsPkgs.cardano-sl-infra
            hsPkgs.cardano-sl-lrc
            hsPkgs.cardano-sl-networking
            hsPkgs.cardano-sl-ssc
            hsPkgs.cardano-sl-sinbin
            hsPkgs.cardano-sl-txp
            hsPkgs.cardano-sl-update
            hsPkgs.cardano-sl-util
            hsPkgs.cborg
            hsPkgs.conduit
            hsPkgs.containers
            hsPkgs.cryptonite
            hsPkgs.data-default
            hsPkgs.deepseq
            hsPkgs.directory
            hsPkgs.ekg-core
            hsPkgs.ether
            hsPkgs.exceptions
            hsPkgs.filepath
            hsPkgs.formatting
            hsPkgs.lens
            hsPkgs.log-warper
            hsPkgs.mtl
            hsPkgs.random
            hsPkgs.reflection
            hsPkgs.rocksdb-haskell-ng
            hsPkgs.pipes
            hsPkgs.safe-exceptions
            hsPkgs.serokell-util
            hsPkgs.stm
            hsPkgs.text
            hsPkgs.formatting
            hsPkgs.time-units
            hsPkgs.transformers
            hsPkgs.universum
            hsPkgs.unliftio
            hsPkgs.unordered-containers
          ];
          build-tools = [ hsPkgs.cpphs ];
        };
        tests = {
          test = {
            depends  = [
              hsPkgs.base
              hsPkgs.bytestring
              hsPkgs.cardano-sl-binary
              hsPkgs.cardano-sl-binary-test
              hsPkgs.cardano-sl-block
              hsPkgs.cardano-sl-core
              hsPkgs.cardano-sl-core-test
              hsPkgs.cardano-sl-crypto
              hsPkgs.cardano-sl-crypto-test
              hsPkgs.cardano-sl-delegation-test
              hsPkgs.cardano-sl-ssc
              hsPkgs.cardano-sl-txp-test
              hsPkgs.cardano-sl-update
              hsPkgs.cardano-sl-update-test
              hsPkgs.cardano-sl-util
              hsPkgs.cardano-sl-util-test
              hsPkgs.containers
              hsPkgs.formatting
              hsPkgs.generic-arbitrary
              hsPkgs.hspec
              hsPkgs.QuickCheck
              hsPkgs.quickcheck-instances
              hsPkgs.random
              hsPkgs.serokell-util
              hsPkgs.text
              hsPkgs.formatting
              hsPkgs.time-units
              hsPkgs.universum
              hsPkgs.unordered-containers
            ];
            build-tools = [ hsPkgs.cpphs ];
          };
        };
      };
    } // rec { src = ../block; }