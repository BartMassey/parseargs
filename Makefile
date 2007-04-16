test: dist/build/libHSParseArgs-0.1.a test.hs
	ghc --make test

dist/build/libHSParseArgs-0.1.a: ParseArgs.hs
	runhaskell Setup.hs build

install:
	sudo runhaskell Setup.hs install
