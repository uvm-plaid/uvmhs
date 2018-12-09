.PHONY: interact
interact: uvmhs.cabal
	stack ghci uvmhs:lib

.PHONY: build
build: uvmhs.cabal
	stack build

.PHONY: build-profile
build-profile: uvmhs.cabal
	stack build --profile

.PHONY: install
install: uvmhs.cabal
	stack install

.PHONY: configure
configure: uvmhs.cabal

uvmhs.cabal: package.yaml
	hpack --force

.PHONY: clean
clean:
	stack clean
	rm -f uvmhs.cabal
