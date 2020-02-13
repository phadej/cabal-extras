INSTALL_FLAGS=

all :
	@echo done nothing

cabal-diff-test-colour :
	cabal v2-run cabal-diff:cabal-diff colour 2.3.4 2.3.5

install-cabal-env :
	cabal v2-install cabal-env --overwrite-policy=always $(INSTALL_FLAGS)

install-cabal-diff :
	cabal v2-install cabal-diff --overwrite-policy=always $(INSTALL_FLAGS)

install-cabal-deps :
	cabal v2-install cabal-deps --overwrite-policy=always $(INSTALL_FLAGS)

install-cabal-bundler :
	cabal v2-install cabal-bundler --overwrite-policy=always $(INSTALL_FLAGS)

install-cabal-store-check :
	cabal v2-install cabal-store-check --overwrite-policy=always $(INSTALL_FLAGS)

install-cabal-store-gc :
	cabal v2-install cabal-store-gc --overwrite-policy=always $(INSTALL_FLAGS)
