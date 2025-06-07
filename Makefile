LIB_NAME     := uvmhs

ALL_HS_FILES := $(shell find src -name '*.hs')

GHCID_FILE    := src/UVMHSMain.hs
GHCID_E       := dev
GHCID_GHCI_OPTIONS := \
	-v1 \
	-j \
	-fno-hide-source-paths \
	-fobject-code \
	-fno-break-on-exception \
	-fno-break-on-error \
	-ferror-spans \
	-fdiagnostics-color=always
GHCID_OPTIONS := \
	--warnings \
	--clear \
	--run=$(GHCID_E) \
	--command="stack ghci --ghci-options='$(GHCID_GHCI_OPTIONS)'"

####################
# PRIMARY COMMANDS #
####################

.PHONY: build
build: .stack-work
	stack build

.PHONY: test
test: .stack-work
	stack test

.PHONY: run
run: .stack-work
	stack run

.PHONY: docs
docs: .stack-work
	stack haddock
	rm -rf ./docs
	cp -r `stack path --local-doc-root` ./docs

.PHONY: fixity-levels
fixity-levels: fixity-levels.txt

fixity-levels.txt: $(ALL_HS_FILES)
	grep -hE '^infix' $(ALL_HS_FILES) | sed -E "s/^infix([rl]?)[ ]*(.*)$$/\\2 [\\1]/g" | sort > $@
	echo "" >> $@
	echo "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~" >> $@
	echo "" >> $@
	grep -E '^infix' $(ALL_HS_FILES) >> $@

##############
# FULL CLEAN #
##############

.PHONY: clean
clean:
	stack clean --full
	rm -rf .stack-work
	rm -f $(LIB_NAME).cabal
	rm -f stack.yaml.lock
	rm -rf doc
	rm -f fixity-levels.txt

###################
# PREPARE RELEASE #
###################

.PHONY: release
release:
	make build && make test && make docs && make fixity-levels

#########################
# GHCI AND GHCID SPINUP #
#########################

.PHONY: ghci
ghci: .stack-work
	stack ghci

.PHONY: dev
dev: .stack-work
	ghcid $(GHCID_FILE) $(GHCID_OPTIONS)

.stack-work:
	stack setup

#########################
# PROFILING AND TRACING #
#########################

.PHONY: profile
profile: .stack-work
	stack run --profile -- +RTS -p

.PHONY: trace
trace: .stack-work
	stack run --trace

##########
# HOOGLE #
##########

.PHONY: hoogle
hoogle:
	stack hoogle -- generate --local
	(sleep 1 && open http://localhost:8080/?scope=package%3A$(NAME)) &
	stack hoogle -- server --local --port=8080

##############
# LINE COUNT #
##############

.PHONY: count-lines
count-lines:
	find src -name "*.hs" | xargs wc -l

##################
# GITHUB PREVIEW #
##################

.PHONY: local-readme-github
github-preview:
	grip --browser
