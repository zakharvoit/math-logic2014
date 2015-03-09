# This makefile uses ocamlbuild to build all executables

SRC_DIR = src
EXECUTABLES = task4
NATIVES = $(EXECUTABLES:%=$(SRC_DIR)/%.native)
INCLUDE_DIRS = src
TEST_TASKS = task4.test
GENERATE_TASKS = $(TEST_TASKS:%.test=%.generate)

all: build

build: $(NATIVES)

%.native:
	ocamlbuild -Is $(INCLUDE_DIRS) $@

generate: $(GENERATE_TASKS)

test: all $(TEST_TASKS)

%.test: %.native
	./test/test.pl $(@:%.test=%)

%.generate: %.native
	./test/generate.pl $(@:%.generate=%)

clean:
	rm -rf _build
	rm -f *.native

.PHONY: all build clean generate test
