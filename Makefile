build:
	ghc -O3 -o repl *.hs

clean:
	rm *.o
	rm *.hi
