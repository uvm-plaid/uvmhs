E    := UVMHSMain.main

.PHONY: build
build:
	stack build

.PHONY: dev
dev: .stack-work
	ghcid --test $E

.stack-work:
	stack setup

.PHONY: run
eval:
	stack ghci --ghci-options -e --ghci-options $E


.PHONY: run
run:
	stack run

.PHONY: profile
profile:
	stack run --profile -- +RTS -p

.PHONY: trace
trace:
	stack run --profile -- +RTS -xc

.PHONY: ghci
ghci:
	stack ghci

.PHONY: docs
docs:
	stack haddock
	rm -rf ./docs
	cp -r `stack path --local-doc-root` ./docs

.PHONY: clean
clean:
	stack clean --full
	rm -f $(NAME).cabal
	rm -rf doc

.PHONY: hoogle
hoogle:
	stack hoogle -- generate --local
	(sleep 1 && open http://localhost:8080/?scope=package%3A$(NAME)) &
	stack hoogle -- server --local --port=8080
