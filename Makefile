BUILD_PATH = _build/default
PROJECTS = $(shell find . -maxdepth 2 -type f -regextype egrep -regex '\./[0-9]{2}/.*\.ml' -printf "%P\n" | sort)
EXECUTABLES = $(PROJECTS:%.ml=%.exe)
TARGETS = $(addprefix $(BUILD_PATH)/, $(EXECUTABLES))

ifdef TASK
    INSPECT = $(patsubst %.ml,%.exe,$(wildcard $(TASK)/*.ml))
else
    INSPECT = $(lastword $(EXECUTABLES))
endif

EXAMPLES = $(sort $(patsubst %.in,%,$(wildcard $(dir $(INSPECT))example_*.in)))

ifdef PART
    EXAMPLE = $(dir $(INSPECT))example_$(PART)
else
    EXAMPLE = $(lastword $(EXAMPLES))
endif

DUNE = dune
DUNEOPT = --no-print-directory

all: $(TARGETS) answer

$(TARGETS): $(BUILD_PATH)/%.exe: %.ml
	$(DUNE) build $(DUNEOPT) $*.exe

$(EXECUTABLES): %.exe: $(BUILD_PATH)/%.exe

answer: $(INSPECT)
	$(DUNE) exe $< $(addprefix $(dir $<),input)

verify: $(INSPECT) $(EXAMPLE).in $(EXAMPLE).out
	@$(DUNE) exe $< $(EXAMPLE).in \
		| grep -Ev '^#' \
		| diff --color=always -u /dev/stdin $(EXAMPLE).out

clean:
	$(DUNE) clean $(DUNEOPT)

.PHONY:         \
	all     \
	clean
