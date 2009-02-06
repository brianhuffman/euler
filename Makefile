all: AllEuler

test: AllEuler
	time ./AllEuler | tee test

stat: AllEuler
	time ./AllEuler +RTS -s
	cat AllEuler.stat

AllEuler: *.hs
	ghc --make -O AllEuler.hs -o AllEuler

clean:
	rm *.o *.hi
