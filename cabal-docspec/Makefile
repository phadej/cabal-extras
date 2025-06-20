VERSION=0.0.0.20250606
DATE="June 6th, 2025"
EXETARGET=cabal-docspec

cabal-docspec.1 : MANUAL.md
	echo '.TH CABAL-DOCSPEC 1 "$(DATE)" "cabal-docspec $(VERSION)" "Cabal Extras"' > cabal-docspec.1
	pandoc -f markdown -t man MANUAL.md >> cabal-docspec.1

man : cabal-docspec.1
	cat cabal-docspec.1 | man -l -

ALPINEVERSION:=3.17.3
GHCUPVERSION:=0.1.50.2
GHCVERSION:=9.8.4
CABALVERSION:=3.10.3.0

CABALPLAN:=$(HOME)/.local/bin/cabal-plan
CABAL:=$(HOME)/.ghcup/bin/cabal
GHC:=$(HOME)/.ghcup/bin/ghc-$(GHCVERSION)
GHCUP:=$(HOME)/.ghcup/bin/ghcup

# docker run -ti -v $(dirname $(pwd)):/src alpine:3.17.3
# cd /src/cabal-docspec
# apk add make
# make alpine-release
#
.PHONY: alpine-release
alpine-release :
	apk add binutils-gold curl gcc g++ git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev openssh-client perl tar tmux vim xz xz-dev zlib-dev zlib-static
	mkdir -p $(HOME)/.local/bin
	curl -L https://github.com/haskell-hvr/cabal-plan/releases/download/v0.7.3.0/cabal-plan-0.7.3.0-x86_64-linux.xz | xz -d > $(CABALPLAN)
	chmod a+x $(CABALPLAN)
	mkdir -p $(HOME)/.ghcup/bin
	curl https://downloads.haskell.org/~ghcup/$(GHCUPVERSION)/x86_64-linux-ghcup-$(GHCUPVERSION) > $(GHCUP)
	chmod a+x $(GHCUP)
	$(GHCUP) install ghc $(GHCVERSION)
	$(GHCUP) install cabal $(CABALVERSION)
	$(CABAL) update --ignore-project
	$(CABAL) build exe:$(EXETARGET) -fexe --with-compiler $(GHC) --enable-executable-static
	strip $$($(CABALPLAN) list-bin $(EXETARGET))
	@ls -l $$($(CABALPLAN) list-bin $(EXETARGET))
	cat $$($(CABALPLAN) list-bin $(EXETARGET)) | xz > $(EXETARGET)-$(VERSION)-x86_64-linux.xz
	@ls -l $(EXETARGET)-$(VERSION)-x86_64-linux.xz
	sha256sum $(EXETARGET)-$(VERSION)-x86_64-linux.xz | tee $(EXETARGET)-$(VERSION).SHA256SUM
