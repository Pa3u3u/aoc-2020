BUILD_PATH = _build/default
PROJECTS = $(shell find . -maxdepth 2 -type f -regextype egrep -regex '\./[0-9]{2}/.*\.ml' -printf "%P\n")
EXECUTABLES = $(PROJECTS:%.ml=%.exe)
TARGETS = $(addprefix $(BUILD_PATH)/, $(EXECUTABLES))

DUNE = dune
DUNEOPT = --no-print-directory

all: $(TARGETS) answer

$(TARGETS): $(BUILD_PATH)/%.exe: %.ml
	$(DUNE) build $(DUNEOPT) $*.exe

$(EXECUTABLES): %.exe: $(BUILD_PATH)/%.exe

answer: $(lastword $(EXECUTABLES))
	$(DUNE) exe $< $(addprefix $(dir $^), input)

clean:
	$(DUNE) clean $(DUNEOPT)

.PHONY:         \
	all     \
	clean
