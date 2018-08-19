MODULE = __MODULE__
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
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

lint: checkdoc

clean:
	rm -f $(OBJECTS)

cleanall: clean
	rm -rf $(ELPA_DIR)

melpa: version
	$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version
	$(EMACSRUN) -L ~/VC/markdown-mode/ \
		--load tests/*.el \
		--file tests/poly-markdown-tests.el 

test: version
	$(EMACSBATCH) --load targets/melpa.el --load targets/test.el

version:
	@echo "******************* Testing $(MODULE) *************************"
	@$(EMACS) --version

