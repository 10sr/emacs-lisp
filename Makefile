emacs ?= emacs
git ?= git
markdown ?= markdown

all: compile clean

.PHONY: all

el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

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
# gh-pages

.PHONY: gh-pages gh-pages-push git-user-config

gh_pages_branch := gh-pages
gh_pages_base_branch := master
gh_pages_remote := origin

git_user_name ?= 10sr
git_user_email ?= 8slashes+git@gmail.com

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
	$(git) commit -m 'Update index.html'
	$(git) checkout -f -

git-user-config:
	test -n "`$(git config user.name)`" || git config user.name $(git_user_name)
	test -n "`$(git config user.email)`" || git config user.name $(git_user_email)
