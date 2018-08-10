export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
POLYMODE_VERSION = $(git describe --tags --abbrev=0 | sed 's/^v//')
MELPA_DIR := MELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(MELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

# export PM_VERBOSE

.PHONY: test version compile

all: compile checkdoc test

build: version clean
	$(EMACSBATCH) --funcall batch-byte-compile *.el

checkdoc: version
	$(EMACSBATCH) --load targets/checkdoc.el

clean:
	rm -f $(OBJECTS)

cleanall: clean
	rm -rf $(MELPA_DIR)

lint: checkdoc

$(MELPA_DIR):
	$(EMACSBATCH) --load targets/melpa.el

melpa: version $(MELPA_DIR)

start: version
	$(EMACSRUN) -L ~/VC/markdown-mode/ \
		--load tests/*.el \
		--file tests/poly-markdown-tests.el 

# test: version
# 	$(EMACSBATCH) \
# 		--load targets/melpa.el \
# 		--load tests/*tests.el \
# 		--funcall ert-run-tests-batch-and-exit

test: version
	$(EMACSBATCH) -L ~/VC/markdown-mode/ \
		--load tests/*tests.el \
		--funcall ert-run-tests-batch-and-exit

version:
	@echo POLYMODE: $(VERSION)
	@$(EMACS) --version

