# This makefile uses ocamlbuild to build all executables

SRC_DIR = src
EXECUTABLES = task1
NATIVES = $(EXECUTABLES:%=$(SRC_DIR)/%.native)
INCLUDE_DIRS = src
GENERATE_TASKS = task1.generate
TEST_TASKS = task1.test

all: build

build: $(NATIVES)

%.native:
	ocamlbuild -Is $(INCLUDE_DIRS) $@

generate: $(GENERATE_TASKS)

test: $(TEST_TASKS)

%.test:
	./test/test.pl $(@:%.test=%)

%.generate:
	./test/generate.pl $(@:%.generate=%)

clean:
	rm -rf _build
	rm -f *.native

.PHONY: all build clean generate test
