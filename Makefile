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
# Build packages from current revision and make commit to gh-pages

.PHONY: gh-pages gh-pages-push

gh_pages_branch := gh-pages
gh_pages_push_target := git@github.com:10sr/emacs-lisp.git
git_current_branch := $(shell git symbolic-ref --short 2>/dev/null)
git_current_revision := $(shell $(git) rev-parse HEAD)

git_user_name ?= $(shell git config user.name || echo 10sr)
git_user_email ?= $(shell git config user.email || echo 8slashes+git@gmail.com)

gh-pages-push: gh-pages
	$(git) push $(gh_pages_push_target) $(gh_pages_branch)

gh-pages: archive-all
	# check working tree and index are clean
	$(git) diff --exit-code
	$(git) diff --cached --exit-code

	# Create gh-pages branch
	$(git) branch $(gh_pages_branch) remotes/$(gh_pages_push_target)/$(gh_pages_branch) || \
		$(git) branch $(gh_pages_branch) || true

	$(git) checkout -f $(gh_pages_branch)
	$(RM) -r p && $(MKDIR) p
	$(CP) packages/* p
	$(git) add p/*
	$(git) diff --cached --exit-code || \
		$(git) commit -m 'Packages build from $(git_current_revision)'
ifneq ($(gh_pages_branch),$(git_current_branch))
	$(git) checkout -f -
endif


######################################3
# Archives

# Targets:
#     archive-all: Build package archives in packages/ dir
#     archive-clean: Remove all files under packages/ dir

recipes := $(wildcard recipes/*)
archives := $(recipes:recipes/%=%)

.PHONY: archive $(archives) archive-all archive-clean

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

archive-all: archive-clean libs/package-build.el
	$(emacs) -batch -Q \
		--load libs/package-build.el \
		--eval '(setq package-build-working-dir (concat "$(project_root)" "/working/"))' \
		--eval '(setq package-build-archive-dir (concat "$(project_root)" "/packages/"))' \
		--eval '(setq package-build-recipes-dir (concat "$(project_root)" "/recipes/"))' \
		--eval '(package-build-all)'

archive-clean:
	$(RM) -r $(project_root)/packages


libs/package-build.el:
	mkdir -p libs
	curl -sSL https://github.com/milkypostman/melpa/raw/master/package-build.el \
		>libs/package-build.el
