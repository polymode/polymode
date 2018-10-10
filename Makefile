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
	@$(EMACSBATCH) --load targets/melpa.el --load elisp-lint.el \
		--funcall elisp-lint-files-batch --no-package-format --no-fill-column $(LINTELS)

melpa: version
	@$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version
	$(EMACSRUN) -L . \
		--load targets/melpa.el
		--load tests/*.el

test: version
	@echo "******************* TESTING $(MODULE) **************************"
	@$(EMACSBATCH) --load targets/melpa.el --load targets/test.el

version:
	@echo "EMACS VERSION: $(EMACS_VERSION)"

template../%:
	@echo $@
	$(eval OUTDIR := $(subst template, , $@))
	$(eval ABSDIR := $(abspath $(OUTDIR)))
	./targets/template.sh $(ABSDIR)

update-versions:
	@echo "******************* UPDATING VERSIONS **************************"
	@./targets/update-versions.sh
