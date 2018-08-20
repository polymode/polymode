MODULE = POLYMODE
export EMACS ?= emacs
EMACS_VERSION = $(shell ${EMACS} -Q --batch --eval "(princ emacs-version)")
ELPA_DIR := ELPA/$(EMACS_VERSION)
EMACSRUN = $(EMACS) -Q -L . -L modes -L tests -L $(ELPA_DIR)
EMACSBATCH = $(EMACSRUN) --batch

ELS = $(wildcard *.el)
OBJECTS = $(ELS:.el=.elc)

# export PM_VERBOSE

.PHONY: build test version clean cleansilent cleanall

all: build checkdoc test

build: version cleansilent
	@echo "******************* BUILDING $(MODULE) *************************"
	@$(EMACSBATCH) --funcall batch-byte-compile *.el

checkdoc: version
	@echo "******************* CHECKDOC $(MODULE) *************************"
	@$(EMACSBATCH) --load targets/checkdoc.el

clean:
	rm -f $(OBJECTS)

cleanall: cleansilent
	rm -rf $(ELPA_DIR)

cleansilent:
	@rm -f $(OBJECTS)

deploy:
	cd ../polymode.github.io/; mkdocs gh-deploy --config-file ../polymode/mkdocs.yml --remote-branch master

lint: checkdoc

melpa: version
	$(EMACSBATCH) --load targets/melpa.el

elpa: melpa

start: version
	$(EMACSRUN) -L ~/VC/markdown-mode/ \
		--load tests/*.el \
		--file tests/poly-markdown-tests.el 

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
