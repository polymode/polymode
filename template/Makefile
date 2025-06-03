MODULE = __MODULE__
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := .ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch
EMACSTARGET = $(EMACSBATCH) -l targets/poly-targets.el

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)
LINTELS = $(filter-out __MODULE__-autoloads.el, $(ELS))

# export PM_VERBOSE

.PHONY: all build checkdoc lint clean cleanall melpa elpa start test version

all: build checkdoc test

build: version clean
	@echo "******************* BUILDING $(MODULE) *************************"
	$(EMACSTARGET) -f pm--target-melpa-init -f batch-byte-compile *.el

build-local: version clean
	@echo "******************* BUILDING $(MODULE) *************************"
	$(EMACSTARGET) -f pm--target-local-init --funcall batch-byte-compile *.el

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	$(EMACSTARGET) -f pm--target-checkdoc

lint: version
	@$(EMACSTARGET) -f pm--target-melpa-init -f pm--target-lint $(LINTELS)

clean:
	rm -f $(OBJECTS)

cleanall: clean
	rm -rf $(ELPA_DIR) *autoloads.el

melpa: version
	$(EMACSTARGET) -f pm--target-melpa

elpa: melpa

start: version melpa
	$(EMACSTARGET) -L . -f pm--target-melpa-init --load tests/*.el

startvs: version
	$(EMACSTARGET) -L . -f pm--target-local-init \
		       --load tests/*.el --load ~/.eBasic.el

test: build version
	@echo "******************* Testing $(MODULE) ***************************"
	$(EMACSTARGET) -f pm--target-melpa-init -f pm--target-test

test-local: version
	@echo "******************* Testing $(MODULE) ***************************"
	$(EMACSTARGET) -f pm--target-local-init -f pm--target-test

version:
	@echo "EMACS VERSION: $(EMACS_VERSION)"
	@echo "GIT HEAD: $(shell git rev-parse --short HEAD)"

