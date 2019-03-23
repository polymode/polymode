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

.PHONY: all build checkdoc lint clean cleanall melpa elpa start test version

all: build checkdoc test

build: version clean
	@echo "******************* BUILDING $(MODULE) *************************"
	$(EMACSBATCH) --load targets/melpa-init.el --funcall batch-byte-compile *.el

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	$(EMACSBATCH) --load targets/checkdoc.el

lint: version
	@$(EMACSBATCH) --load targets/melpa-init.el --load targets/lint.el $(LINTELS)

clean:
	rm -f $(OBJECTS)

cleanall: clean
	rm -rf $(ELPA_DIR) *autoloads.el

melpa: version
	$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version melpa
	$(EMACSRUN) -L . \
		--load targets/melpa-init.el \
		--load tests/*.el

startvs: version
	$(EMACSRUN) -L . \
		--load targets/local.el \
		--load tests/*.el --load ~/.eBasic.el

test: build version
	@echo "******************* Testing $(MODULE) ***************************"
	$(EMACSBATCH) --load targets/melpa-init.el --load targets/test.el

version:
	@echo "EMACS VERSION: $(EMACS_VERSION)"
	@echo "GIT HEAD: $(shell git rev-parse --short HEAD)"

