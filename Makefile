E    := UVMHSMain.main

.PHONY: build
build:
	stack build

.PHONY: dev
dev: .stack-work
	ghcid \
		--warnings \
		--clear \
		--run=$E \
		--command=" \
			stack ghci uvmhs:lib \
				--ghci-options=' \
					-v1 \
					-j \
					-fno-hide-source-paths \
					-fobject-code \
					-fno-break-on-exception \
					-fno-break-on-error \
					-ferror-spans \
					-fdiagnostics-color=always'"

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
	stack run --trace

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

ALL_HS_FILES := $(shell find src -name '*.hs')

fixity-levels.txt: Makefile $(ALL_HS_FILES)
	grep -hE '^infix' $(ALL_HS_FILES) | sed -E "s/^infix([rl]?)[ ]*(.*)$$/\\2 [\\1]/g" | sort > $@
	echo "" >> $@
	echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" >> $@
	echo "" >> $@
	grep -E '^infix' $(ALL_HS_FILES) >> $@

.PHONY: count-lines
count-lines:
	find src -name "*.hs" | xargs wc -l

.PHONY: local-readme-github
github-preview:
	grip --browser

