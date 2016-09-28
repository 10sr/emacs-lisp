emacs ?= emacs
git ?= git
markdown ?= markdown

all: compile clean

.PHONY: all

el = $(wildcard el/*.el)
elc = $(el:%.el=%.elc)
project_root := $(PWD)

clean:
	$(RM) $(elc)



#####################################
# Testing

.PHONY: test compile info

test: compile info

compile: $(elc)

$(elc): %.elc: %.el
	$(emacs) -batch -Q -f batch-byte-compile $<


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
	$(emacs) -batch -Q \
		--eval "(require 'package)" \
		--eval "$(elisp_print_infos)" \
		$^


elpa:
	CASK_EMACS=$(emacs) cask exec github-elpa update

cask-install:
	CASK_EMACS=$(emacs) cask install
