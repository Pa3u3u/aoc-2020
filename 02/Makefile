SOURCES = $(wildcard *.ml)
BUILD_DIR = _build/default
TARGETS = $(SOURCES:%.ml=$(BUILD_DIR)/%.exe)

DUNE ?= dune

all: $(TARGETS)

$(TARGETS): $(BUILD_DIR)/%.exe: %.ml
	$(DUNE) build $*.exe

ifeq ($(words $(SOURCES)), 1)
answer: $(TARGETS) input
	$(TARGETS) input
endif

clean:
	$(RM) -r _build
	$(RM) dune-project

.PHONY:         \
	all     \
	test    \
	clean
