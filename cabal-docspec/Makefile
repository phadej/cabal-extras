VERSION=0.0.0.20211114

cabal-docspec.1 : MANUAL.md
	echo '.TH CABAL-DOCSPEC 1 "January 15, 2021" "cabal-docspec $(VERSION)" "Cabal Extras"' > cabal-docspec.1
	pandoc -f markdown -t man MANUAL.md >> cabal-docspec.1

man : cabal-docspec.1
	cat cabal-docspec.1 | man -l -

CABALPLAN:=$(HOME)/.cabal/bin/cabal-plan
CABAL:=$(HOME)/.ghcup/bin/cabal
GHC:=$(HOME)/.ghcup/bin/ghc-8.8.4
GHCUP:=$(HOME)/.ghcup/bin/ghcup

# docker run -ti -v $(pwd):/src alpine:3.14
# cd /src
# apk add make
.PHONY: alpine-release
alpine-release :
	apk add binutils-gold curl gcc git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev openssh-client perl tar tmux vim xz zlib-dev zlib-static
	mkdir -p $(HOME)/.ghcup/bin
	curl https://downloads.haskell.org/~ghcup/0.1.17.4/x86_64-linux-ghcup-0.1.17.4 > $(GHCUP)
	chmod a+x $(GHCUP)
	$(GHCUP) install ghc 8.8.4
	$(GHCUP) install cabal 3.6.2.0
	$(CABAL) update --ignore-project
	$(CABAL) install --ignore-project cabal-plan -fexe --with-compiler $(GHC)
	$(CABAL) build cabal-docspec --with-compiler $(GHC) --enable-executable-static
	strip $$($(CABALPLAN) list-bin cabal-docspec)
	@ls -l $$($(CABALPLAN) list-bin cabal-docspec)
	cat $$($(CABALPLAN) list-bin cabal-docspec) | xz > cabal-docspec-$(VERSION).xz
	@ls -l cabal-docspec-$(VERSION).xz
	sha256sum cabal-docspec-$(VERSION).xz
