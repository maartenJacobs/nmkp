install: build
	stack install

run: build
	stack exec nmkp

build:
	stack build

lint:
	hlint -g
