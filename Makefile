build:
	ghc -O3 -o repl src/*.hs

clean:
	rm src/*.o
	rm src/*.hi
