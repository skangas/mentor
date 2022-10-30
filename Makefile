EMACS=emacs
EMACS_BATCH = $(EMACS) -Q -batch
ELPADIR = $(HOME)/.emacs.d/elpa
SUBDIRS = $(shell find $(ELPADIR) -mindepth 1 -maxdepth 1 -type d)
EMACS_LOADPATH = -L . $(patsubst %,-L %, $(SUBDIRS)) -L ../url-scgi
EMACS_LOAD = $(EMACS_BATCH) $(EMACS_LOADPATH)
TARGET=$(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all clean tags test check-declare
.PRECIOUS: %.elc

all: $(TARGET)

%.elc: %.el
	@echo "Compiling $<"
	@$(EMACS_LOAD) -f batch-byte-compile $<

check-declare:
	@$(EMACS_LOAD) --eval '(check-declare-directory default-directory)'

clean:
	rm -f $(TARGET) TAGS *.elc test/*.elc

tags:
	etags mentor*.el

test: all check-declare
	@$(EMACS_LOAD) -l test/mentor-rpc-tests.el -f ert-run-tests-batch-and-exit
	@$(EMACS_LOAD) -l test/mentor-tests.el -f ert-run-tests-batch-and-exit
