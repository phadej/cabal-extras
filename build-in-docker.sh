#!/bin/sh

# Run this script with
#
#   sh build-in-docker.sh
#
# To produce a simple bindist in dist-newstyle/bindist

set -ex

if [ "x$DOCKER" = "xYES" ]; then
  # Install cabal-plan
  mkdir -p "$HOME/.cabal/bin"
  curl -sL https://github.com/haskell-hvr/cabal-plan/releases/download/v0.6.2.0/cabal-plan-0.6.2.0-x86_64-linux.xz > cabal-plan.xz
  echo "de73600b1836d3f55e32d80385acc055fd97f60eaa0ab68a755302685f5d81bc  cabal-plan.xz" | sha256sum -c -
  xz -d < cabal-plan.xz > "$HOME/.cabal/bin/cabal-plan"
  rm -f cabal-plan.xz
  chmod a+x "$HOME/.cabal/bin/cabal-plan"

  cd /build
  cabal update

  cd /build/src
  cabal build --builddir=/build/builddir all

  TARGETS="cabal-bundler cabal-deps cabal-diff cabal-env cabal-store-check cabal-store-gc"
  VERSION=snapshot-$(date +'%Y%m%d')

  for TARGET in $TARGETS; do
  cp "$(cabal-plan list-bin --builddir=/build/builddir "$TARGET")" "/build/bindist/$TARGET"
  strip "/build/bindist/$TARGET"
  xz -c < "/build/bindist/$TARGET" > "/build/bindist/$TARGET-$VERSION-x86_64-linux.xz"
  done

  ls -lh /build/bindist

else

  mkdir -p dist-newstyle/bindist
  docker run --rm -ti -e DOCKER=YES -v "$(pwd):/build/src:ro" -v "$(pwd)/dist-newstyle/bindist:/build/bindist" phadej/ghc:8.6.5-xenial sh /build/src/build-in-docker.sh
  cd dist-newstyle/bindist
  sha256sum cabal-*.xz > SHA256SUMS
  gpg2 --sign --detach-sig --armor SHA256SUMS

fi
