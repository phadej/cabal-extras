VERSION=0.0.0.20201228

cabal-docspec.1 : MANUAL.md
	echo '.TH CABAL-DOCSPEC 1 "December 26, 2020" "cabal-docspec $(VERSION)" "Cabal Extras"' > cabal-docspec.1
	pandoc -f markdown -t man MANUAL.md >> cabal-docspec.1
