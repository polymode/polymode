MODULE = __MODULE__
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := .ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)
LINTELS = $(filter-out __MODULE__-autoloads.el, $(ELS))

# export PM_VERBOSE

.PHONY: test version compile

all: compile checkdoc test

build: version clean
	@echo "******************* BUILDING $(MODULE) *************************"
	$(EMACSBATCH) --funcall batch-byte-compile *.el

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	$(EMACSBATCH) --load targets/checkdoc.el

lint: version
	@$(EMACSBATCH) --load targets/melpa.el --load elisp-lint.el \
		--funcall elisp-lint-files-batch --no-package-format --no-fill-column $(LINTELS)

clean:
	rm -f $(OBJECTS)

cleanall: clean
	rm -rf $(ELPA_DIR) *autoloads.el

melpa: version
	$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version
	$(EMACSRUN) -L ~/VC/markdown-mode/ \
		--load tests/*.el \
		--file tests/poly-markdown-tests.el 

test: version
	@echo "******************* Testing $(MODULE) ***************************"
	$(EMACSBATCH) --load targets/melpa.el --load targets/test.el

version:
	@echo "EMACS VERSION: $(EMACS_VERSION)"

