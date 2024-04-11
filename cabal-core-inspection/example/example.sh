#!/bin/sh

THISDIR=$(pwd)
cd ../ || exit

cabal build cabal-core-inspection:exe:cabal-core-inspection || exit
COREINSP=$(cabal-plan list-bin cabal-core-inspection)

cd "$THISDIR" || exit

cabal build
$COREINSP -w ghc-9.8.2
