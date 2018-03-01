build: Nmkp.hs assets/cow.png assets/grass.png
	ghc -O2 -threaded Nmkp.hs

run: build
	./Nmkp
