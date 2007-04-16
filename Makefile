test: dist/build/ParseArgs.o
	ghc --make test

dist/build/ParseArgs.o: ParseArgs.hs
	runhaskell Setup.hs build

install:
	sudo runhaskell Setup.hs install
