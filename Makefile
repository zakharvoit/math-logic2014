# This makefile uses ocamlbuild to build all executables

SRC_DIR = src
EXECUTABLES = task1
NATIVES = $(EXECUTABLES:%=$(SRC_DIR)/%.native)
INCLUDE_DIRS = src

all: build

build: $(NATIVES)

%.native:
	ocamlbuild -Is $(INCLUDE_DIRS) $@

clean:
	rm -rf _build
	rm -f *.native

.PHONY: all build clean
