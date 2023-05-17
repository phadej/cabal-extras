#!/bin/sh
# This file is generated with cabal-bundler

set -ex

cat <<EOF > SHA256SUMS
f0862eca5ef06da6e6a592c452a747a953e52adce6f66246d3c7a0aa458dfe35  Cabal-3.0.0.0.cabal
5143ec26d740c1a508c93a8860e64407e7546c29b9817db20ff1595c1968d287  Cabal-3.0.0.0.tar.gz
31397cff165772b6c3725583cd45e535145945ad7dd5251a79342c84cc4726ac  ansi-terminal-0.10.1.tar.gz
fb737bc96e2aef34ad595d54ced7a73f648c521ebcb00fe0679aff45ccd49212  ansi-wl-pprint-0.6.9.cabal
a7b2e8e7cd3f02f2954e8b17dc60a0ccd889f49e2068ebb15abfa1d42f7a4eac  ansi-wl-pprint-0.6.9.tar.gz
a9595b2bd73aefebafdd358564bfe5a78aafab29b5d62ff43eb0fe428f0e1d1e  cabal-fmt-0.1.1.tar.gz
3b8d471979617dce7c193523743c9782df63433d8e87e3ef6d97922e0da104e7  colour-2.3.5.tar.gz
29ff6146aabf54d46c4c8788e8d1eadaea27c94f6d360c690c5f6c93dac4b07e  optparse-applicative-0.15.1.0.cabal
4db3675fd1e0594afdf079db46f4cd412d483835d703e7c07e1a1a37d6f046f3  optparse-applicative-0.15.1.0.tar.gz
da67cf11515da751b32a8ce6e96549f7268f7c435769ad19dc9766b69774620b  transformers-compat-0.6.5.tar.gz
EOF

curl --silent --location --output Cabal-3.0.0.0.cabal 'http://hackage.haskell.org/package/Cabal-3.0.0.0/revision/1.cabal'
curl --silent --location --output Cabal-3.0.0.0.tar.gz 'http://hackage.haskell.org/package/Cabal-3.0.0.0/Cabal-3.0.0.0.tar.gz'
curl --silent --location --output ansi-terminal-0.10.1.tar.gz 'http://hackage.haskell.org/package/ansi-terminal-0.10.1/ansi-terminal-0.10.1.tar.gz'
curl --silent --location --output ansi-wl-pprint-0.6.9.cabal 'http://hackage.haskell.org/package/ansi-wl-pprint-0.6.9/revision/4.cabal'
curl --silent --location --output ansi-wl-pprint-0.6.9.tar.gz 'http://hackage.haskell.org/package/ansi-wl-pprint-0.6.9/ansi-wl-pprint-0.6.9.tar.gz'
curl --silent --location --output cabal-fmt-0.1.1.tar.gz 'http://hackage.haskell.org/package/cabal-fmt-0.1.1/cabal-fmt-0.1.1.tar.gz'
curl --silent --location --output colour-2.3.5.tar.gz 'http://hackage.haskell.org/package/colour-2.3.5/colour-2.3.5.tar.gz'
curl --silent --location --output optparse-applicative-0.15.1.0.cabal 'http://hackage.haskell.org/package/optparse-applicative-0.15.1.0/revision/1.cabal'
curl --silent --location --output optparse-applicative-0.15.1.0.tar.gz 'http://hackage.haskell.org/package/optparse-applicative-0.15.1.0/optparse-applicative-0.15.1.0.tar.gz'
curl --silent --location --output transformers-compat-0.6.5.tar.gz 'http://hackage.haskell.org/package/transformers-compat-0.6.5/transformers-compat-0.6.5.tar.gz'

sha256sum -c SHA256SUMS
