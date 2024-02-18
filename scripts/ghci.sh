#! /usr/bin/env nix-shell
#! nix-shell -i bash ../shell.nix

ghc --interactive \
  -isrc/ \
  -itests/ \
  -XDeriveGeneric \
  -XDerivingVia \
  -XFlexibleContexts \
  -XFlexibleInstances \
  -XImportQualifiedPost \
  -XLambdaCase \
  -XMultiParamTypeClasses \
  -XNoImplicitPrelude \
  -XOverloadedStrings \
  -XRecordWildCards \
  -XScopedTypeVariables \
  -XTypeApplications \
  -XTypeOperators \
  -XUndecidableInstances \
  -Wall \
  -ghci-script scripts/ghci.conf
