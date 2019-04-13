EMACS=emacs
EMACS_BATCH = $(EMACS) -Q -batch
ELPADIR = $(HOME)/.emacs.d/elpa
SUBDIRS = $(shell find $(ELPADIR) -mindepth 1 -maxdepth 1 -type d)
EMACS_LOADPATH = -L . $(patsubst %,-L %, $(SUBDIRS))
EMACS_LOAD = $(EMACS_BATCH) $(EMACS_LOADPATH)
TARGET=$(patsubst %.el,%.elc,$(wildcard *.el))

.PHONY: all clean tags test
.PRECIOUS: %.elc

all: $(TARGET)

%.elc: %.el
	@$(EMACS_LOAD) -f batch-byte-compile $<

clean:
	rm -f $(TARGET) TAGS

tags:
	etags mentor*.el url-scgi.el

test:
	@$(EMACS_LOAD) -l test/mentor-rpc-tests.el -f ert-run-tests-batch-and-exit
	@$(EMACS_LOAD) -l test/mentor-tests.el -f ert-run-tests-batch-and-exit
	@$(EMACS_LOAD) -l test/url-scgi-tests.el -f ert-run-tests-batch-and-exit
