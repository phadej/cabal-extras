VERSION=0.0.0.20230406
EXETARGET=cabal-docspec

cabal-docspec.1 : MANUAL.md
	echo '.TH CABAL-DOCSPEC 1 "April 6th, 2023" "cabal-docspec $(VERSION)" "Cabal Extras"' > cabal-docspec.1
	pandoc -f markdown -t man MANUAL.md >> cabal-docspec.1

man : cabal-docspec.1
	cat cabal-docspec.1 | man -l -

CABALPLAN:=$(HOME)/.local/bin/cabal-plan
CABAL:=$(HOME)/.ghcup/bin/cabal
GHC:=$(HOME)/.ghcup/bin/ghc-9.2.7
GHCUP:=$(HOME)/.ghcup/bin/ghcup

ALPINEVERSION:=3.17.3
GHCUPVERSION:=0.1.19.2
GHCVERSION:=9.2.7
CABALVERSION:=3.10.1.0

# docker run -ti -v $(dirname $(pwd)):/src alpine:3.17.3
# cd /src/cabal-docspec
# apk add make
# make alpine-release
#
.PHONY: alpine-release
alpine-release :
	apk add binutils-gold curl gcc git gmp-dev libc-dev libffi-dev make musl-dev ncurses-dev openssh-client perl tar tmux vim xz zlib-dev zlib-static
	mkdir -p $(HOME)/.ghcup/bin
	mkdir -p $(HOME)/.local/bin
	curl -L https://github.com/haskell-hvr/cabal-plan/releases/download/v0.7.3.0/cabal-plan-0.7.3.0-x86_64-linux.xz > cabal-plan.xz
	xz -d < cabal-plan.xz > $(CABALPLAN)
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
