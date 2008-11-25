all: AllEuler

test: AllEuler
	./AllEuler

AllEuler: *.hs
	ghc --make -O AllEuler.hs -o AllEuler

clean:
	rm *.o *.hi