emacs ?= emacs
git ?= git
markdown ?= markdown

all: compile clean

.PHONY: all

el = $(wildcard *.el)
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



#########################################
# Package in gh-pages

.PHONY: gh-pages gh-pages-push git-user-config

gh_pages_branch := gh-pages
gh_pages_base_branch := master
gh_pages_remote := git@github.com:10sr/emacs-lisp.git

git_user_name ?= $(shell git config user.name || echo 10sr)
git_user_email ?= $(shell git config user.email || echo 8slashes+git@gmail.com)

gh-pages-push: gh-pages
	$(git) push $(gh_pages_remote) $(gh_pages_branch)

gh-pages: git-user-config
	# check working tree and index are clean
	$(git) diff --exit-code
	$(git) diff --cached --exit-code
	$(git) branch $(gh_pages_branch) remotes/$(gh_pages_remote)/$(gh_pages_branch) || \
		$(git) branch $(gh_pages_branch) $(gh_pages_base_branch) || true
	$(git) checkout -f $(gh_pages_branch)
	$(git) merge $(gh_pages_base_branch)
	$(markdown) README.md >index.html
	$(git) add index.html
	$(git) diff --cached --exit-code || $(git) commit -m 'Update index.html'
	$(git) checkout -f -

git-user-config:
	test -n "`$(git) config user.name`" || git config user.name $(git_user_name)
	test -n "`$(git) config user.email`" || git config user.email $(git_user_email)


######################################3
# Archives

recipes := $(wildcard recipes/*)
archives := $(recipes:recipes/%=%)

.PHONY: archive $(archives) archive-all

archive: $(archives)

# Using archive-all is recommended, since this does not generate
# archive-content file
$(archives): libs/package-build.el
	$(emacs) -batch -Q \
		--load libs/package-build.el \
		--eval '(setq package-build-working-dir (concat "$(project_root)" "/working/"))' \
		--eval '(setq package-build-archive-dir (concat "$(project_root)" "/packages/"))' \
		--eval '(setq package-build-recipes-dir (concat "$(project_root)" "/recipes/"))' \
		--eval '(package-build-archive (quote $@))'

archive-all: libs/package-build.el
	$(emacs) -batch -Q \
		--load libs/package-build.el \
		--eval '(setq package-build-working-dir (concat "$(project_root)" "/working/"))' \
		--eval '(setq package-build-archive-dir (concat "$(project_root)" "/packages/"))' \
		--eval '(setq package-build-recipes-dir (concat "$(project_root)" "/recipes/"))' \
		--eval '(package-build-all)'



libs/package-build.el:
	mkdir -p libs
	curl -sSL https://github.com/milkypostman/melpa/raw/master/package-build.el \
		>libs/package-build.el
