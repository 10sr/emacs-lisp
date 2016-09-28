emacs ?= emacs
casked_emacs := cask exec $(emacs)
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


##############################

cask_install_path := $(project_root)/cask
cask_repository := https://github.com/cask/cask.git
cask_version := v0.8.0

install-cask:
	test -d $(cask_install_path) || $(git) clone $(cask_repository) $(cask_install_path)
	cd $(cask_install_path)
	$(git) checkout -f $(cask_version)
	$(git) clean -xdf


cask-install:
	CASK_EMACS=$(emacs) cask install
