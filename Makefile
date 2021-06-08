SRC=$(SOURCE)
OPT?=0
LINK?=0

HS_DIR=hs
C_DIR=c
TMP_DIR=build

GHC_OPTS=-fasm -fforce-recomp -ddump-to-file -ddump-asm -ddump-simpl -ddump-cmm -ddump-stg
CFLAGS := -Wall -Wextra

phony: info buildc buildhs clean configure

.SILENT: clean

configure:
	mkdir -p $(TMP_DIR)
	# TODO: resolve compiler paths

info:
	clang --version
	opt --version
	llc --version

buildc:
	mkdir -p $(TMP_DIR)
	clang -S -emit-llvm $(C_DIR)/$(SRC).c
	mv $(SRC).ll $(TMP_DIR)/$(SRC).ll
	opt -S -O$(OPT) $(TMP_DIR)/$(SRC).ll -o $(TMP_DIR)/$(SRC)-opt.ll
	llc $(TMP_DIR)/$(SRC)-opt.ll -march=x86-64 -o $(TMP_DIR)/$(SRC).s
ifeq ($(LINK),1)
	clang $(TMP_DIR)/$(SRC).s -Wall -o $(TMP_DIR)/$(SRC)
endif

buildhs:
	mkdir -p $(TMP_DIR)
	cp $(HS_DIR)/$(SRC).hs $(TMP_DIR)/$(SRC).hs
	ghc -c -O$(OPT) $(TMP_DIR)/$(SRC).hs $(GHC_OPTS)
ifeq ($(LINK),1)
	ghc $(TMP_DIR)/$(SRC).o -o $(TMP_DIR)/$(SRC)
endif

build: buildc buildhs

clean:
	find $(TMP_DIR) -type f -delete
