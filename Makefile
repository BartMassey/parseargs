all: test doc/index.html

test: test.hs ParseArgs.hs
	ghc --make -o test test.hs

doc/index.html: ParseArgs.hs
	haddock --html --hoogle -o doc ParseArgs.hs

