MODULE = POLYMODE
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := .ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch
EMACSTARGET = $(EMACSBATCH) -l targets/poly-targets.el
EASK ?= eask

ELS = $(wildcard *.el)
LINTELS = $(filter-out polymode-autoloads.el polymode-compat.el polymode-configuration.el, $(ELS))
OBJECTS = $(ELS:.el=.elc)

# export PM_VERBOSE

.PHONY: build test version clean cleansilent cleanall

all: build checkdoc test

build: cleansilent
	@echo "******************* BUILDING $(MODULE) *************************"
	$(EASK) compile

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	$(EASK) checkdoc

clean:
	$(EASK) clean all

cleanall: cleansilent
	$(EASK) clean all

cleansilent:
	$(EASK) clean elc

docs-build:
	mkdocs build

docs-deploy:
	cd ../polymode.github.io/; mkdocs gh-deploy --config-file ../polymode/mkdocs.yml --remote-branch master

lint: version
	@$(EASK) lint checkdoc
	@$(EASK) lint elisp-lint

melpa: version
	@$(EASK) install-deps --dev

elpa: melpa

start: version melpa
	$(EMACSTARGET) -L . 		 \
		-f pm--target-melpa-init \
		--load tests/*.el

startvs: version
	$(EMACSTARGET) -L . 		 \
		-f pm--target-melpa-init \
		-f pm--parget-local   	 \
		--load tests/*.el --load ~/.eBasic.el

test: version clean
	@echo "******************* TESTING $(MODULE) **************************"
	$(EMACSTARGET) -f pm--target-melpa-init -f pm--target-test

test-local: version
	@echo "******************* Testing $(MODULE) ***************************"
	$(EASK) test ert tests/*.el

test/%:
	$(eval PATTERN := $(subst test/, , $@))
	@echo "********** TESTING WITH PATTERN $(PATTERN) in $(MODULE) ************"
	$(EMACSTARGET) -f pm--target-melpa-init 	       \
		--eval "(setq pm-ert-selector \"$(PATTERN)\")" \
		-f pm--target-test

template../%:
	@echo $@
	$(eval OUTDIR := $(subst template, , $@))
	$(eval ABSDIR := $(abspath $(OUTDIR)))
	./targets/template.sh $(ABSDIR)

version:
	@echo "EMACS VERSION: $(EMACS_VERSION)"
	@echo "GIT HEAD: $(shell git rev-parse --short HEAD)"

update-versions:
	@echo "******************* UPDATING VERSIONS **************************"
	@./targets/update-versions.sh

update-templates:
	@echo "******************* UPDATING TEMPLATES *************************"
	@./targets/update-templates.sh
