.DEFAULT_GOAL := help

SRC=$(SOURCE)
OPT?=0
LINK?=0

ifeq ($(VERBOSE),1)
	VERBOSE:=--verbose --color never --cabal-verbose
else
	VERBOSE=
endif

HS_DIR=hs
C_DIR=c
TMP_DIR=build

GHC_OPTS=-fasm -fforce-recomp -ddump-to-file -ddump-asm -ddump-simpl -ddump-cmm -ddump-stg
CFLAGS := -Wall -Wextra

phony: info clean configure help watch test-app run-app build-app dev-ui build-ui

.SILENT: clean

configure:
	mkdir -p $(TMP_DIR)
	# TODO: resolve compiler paths

info:
	clang --version
	opt --version
	llc --version

buildc: ## build c sample programs
	mkdir -p $(TMP_DIR)
	cp $(C_DIR)/$(SRC).c $(TMP_DIR)/$(SRC).c
	clang -S -emit-llvm $(C_DIR)/$(SRC).c
	mv $(SRC).ll $(TMP_DIR)/$(SRC).ll
	opt -S -O$(OPT) $(TMP_DIR)/$(SRC).ll -o $(TMP_DIR)/$(SRC)-opt.ll
	llc $(TMP_DIR)/$(SRC)-opt.ll -march=x86-64 -o $(TMP_DIR)/$(SRC).s
ifeq ($(LINK),1)
	clang $(TMP_DIR)/$(SRC).s -Wall -o $(TMP_DIR)/$(SRC)
endif

buildhs: ## build haskell sample programs
	mkdir -p $(TMP_DIR)
	cp $(HS_DIR)/$(SRC).hs $(TMP_DIR)/$(SRC).hs
	ghc -c -O$(OPT) $(TMP_DIR)/$(SRC).hs $(GHC_OPTS)
ifeq ($(LINK),1)
	ghc $(TMP_DIR)/$(SRC).o -o $(TMP_DIR)/$(SRC)
endif

build: buildc buildhs

clean: ## clean build artifacts
	find $(TMP_DIR) -type f -delete

build-ui: ## build client
	node_modules/.bin/esbuild ui/main.jsx --bundle --minify --target=es6 --define:process.env.NODE_ENV="\"production\"" --outfile=static/main.js --sourcemap

dev-ui:
	node_modules/.bin/esbuild ui/main.jsx static/main.css --sourcemap --bundle --define:process.env.NODE_ENV="\"development\""  --serve --outdir=static

build-app: ## build binary
	stack build

watch: ## live rebuild
	stack build --file-watch --test --no-run-tests --local-bin-path bin --copy-bins $(VERBOSE)

run-app: ## start application
	stack run

test-app: ## Run application tests
	stack test

help: ## Display this message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(word 1,$(MAKEFILE_LIST)) | sed -e 's/^\(.*\):.*##\(.*\)$$/\1,\2/' | awk 'BEGIN { FS = ","}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
