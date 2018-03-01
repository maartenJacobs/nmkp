run: build
	./Nmkp

build: Nmkp.hs assets/cow.png assets/grass.png
	# https://functor.tokyo/blog/2017-07-28-ghc-warnings-you-should-enable
	ghc -O2 -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates -threaded Nmkp.hs

lint:
	hlint -g