MODULE = POLYMODE
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := .ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch

ELS = $(wildcard *.el)
LINTELS = $(filter-out polymode-autoloads.el polymode-compat.el polymode-configuration.el, $(ELS))
OBJECTS = $(ELS:.el=.elc)

# export PM_VERBOSE

.PHONY: build test version clean cleansilent cleanall

all: build checkdoc test

build: version cleansilent
	@echo "******************* BUILDING $(MODULE) *************************"
	@$(EMACSBATCH) --funcall batch-byte-compile *.el

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	@$(EMACSBATCH) --load targets/checkdoc.el $(LINTELS)

clean:
	rm -f $(OBJECTS) polymode-autoloads.el

cleanall: cleansilent
	rm -rf $(ELPA_DIR)

cleansilent:
	@rm -f $(OBJECTS)

deploy:
	cd ../polymode.github.io/; mkdocs gh-deploy --config-file ../polymode/mkdocs.yml --remote-branch master

lint: version
	@$(EMACSBATCH) --load targets/melpa-init.el --load targets/lint.el $(LINTELS)

melpa: version
	@$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version melpa
	$(EMACSRUN) -L . \
		--load targets/melpa-init.el \
		--load tests/*.el

startvs: version
	$(EMACSRUN) -L . \
		--load targets/melpa-init.el \
		--load targets/local.el \
		--load tests/*.el --load ~/.eBasic.el

test: version clean
	@echo "******************* TESTING $(MODULE) **************************"
	$(EMACSBATCH) --load targets/melpa-init.el --load targets/test.el

test/%:
	$(eval PATTERN := $(subst test/, , $@))
	@echo "********** TESTING WITH PATTERN $(PATTERN) in $(MODULE) ************"
	$(EMACSBATCH) --load targets/melpa-init \
		--eval "(setq pm-ert-selector \"$(PATTERN)\")" \
		--load targets/test.el

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
