TOP := $(dir $(lastword $(MAKEFILE_LIST)))

EMACS=emacs
EMACS_BATCH = $(EMACS) -Q -batch
ELPADIR = $(HOME)/.emacs.d/elpa
SUBDIRS = $(shell find $(ELPADIR) -mindepth 1 -maxdepth 1 -type d)
EMACS_LOADPATH = -L . $(patsubst %,-L %, $(SUBDIRS)) -L ../url-scgi
TARGET=$(patsubst %.el,%.elc,$(wildcard *.el))

### Find libraries

ASYNC_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/async-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(ASYNC_DIR)" ""
  ASYNC_DIR = $(TOP)../async
endif

URL_SCGI_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/url-scgi-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(URL_SCGI_DIR)" ""
  URL_SCGI_DIR = $(TOP)../url-scgi
endif

XML_RPC_DIR ?= $(shell \
  find -L $(ELPA_DIR) -maxdepth 1 -regex '.*/xml-rpc-[.0-9]*' 2> /dev/null | \
  sort | tail -n 1)
ifeq "$(XML_RPC_DIR)" ""
  XML_RPC_DIR = $(TOP)../xml-rpc
endif

EMACS_LOAD = $(EMACS_BATCH) $(EMACS_LOADPATH)
EMACS_LOAD += -L $(ASYNC_DIR)
EMACS_LOAD += -L $(URL_SCGI_DIR)
EMACS_LOAD += -L $(XML_RPC_DIR)

### Targets

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
