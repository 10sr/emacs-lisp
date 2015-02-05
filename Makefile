emacs ?= emacs

el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

all:

test: build

build: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<
