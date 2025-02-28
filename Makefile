project_root := $(PWD)

emacs ?= emacs
eask ?= EASK_EMACS=$(emacs) eask
git ?= git
markdown ?= markdown
uname := $(shell uname)

ifeq (Darwin,$(uname))
tar := gtar
else
tar := tar
endif



all: compile clean

.PHONY: all

el = $(wildcard el/*.el)
elc = $(el:%.el=%.elc)

clean:
	$(RM) $(elc)



#####################################
# Testing

.PHONY: check compile info

check: compile info

compile: $(elc)

$(elc): %.elc: %.el
	$(eask) emacs -batch -q -f batch-byte-compile $<


elisp_get_file_package_info := \
	(lambda (f) \
		(with-temp-buffer \
			(insert-file-contents-literally f) \
			(package-buffer-info)))

elisp_print_infos := \
	(mapc \
		(lambda (f) \
			(message \"Loading info: %s\" f) \
			(message \"%S\" (funcall $(elisp_get_file_package_info) f))) \
		command-line-args-left)

info: $(el)
	$(eask) emacs -batch -Q \
		--eval "(require 'package)" \
		--eval "$(elisp_print_infos)" \
		$^


elpa:
	$(eask) exec github-elpa update --tar $(tar)

build:
	$(eask) exec github-elpa build --tar $(tar)


install-deps:
	$(eask) install-deps
