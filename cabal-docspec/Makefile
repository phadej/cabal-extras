VERSION=0.0.0.20210111

cabal-docspec.1 : MANUAL.md
	echo '.TH CABAL-DOCSPEC 1 "January 10, 2021" "cabal-docspec $(VERSION)" "Cabal Extras"' > cabal-docspec.1
	pandoc -f markdown -t man MANUAL.md >> cabal-docspec.1

man : cabal-docspec.1
	cat cabal-docspec.1 | man -l -

.PHONY: release
release : cabal-docspec.1
	cabal build cabal-docspec
	strip $$(cabal-plan list-bin cabal-docspec)
	@ls -l $$(cabal-plan list-bin cabal-docspec)
	cat $$(cabal-plan list-bin cabal-docspec) | xz > cabal-docspec-$(VERSION).xz
	@ls -l cabal-docspec-$(VERSION).xz
	sha256sum cabal-docspec-$(VERSION).xz
