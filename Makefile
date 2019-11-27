all :
	@echo done nothing

cabal-diff-test-colour :
	cabal v2-run cabal-diff:cabal-diff colour 2.3.4 2.3.5
