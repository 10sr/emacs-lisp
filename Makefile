emacs ?= emacs
git ?= git
markdown ?= markdown

el = $(wildcard *.el)
elc = $(el:%.el=%.elc)

all: $(elc)

.PHONY: all test build info clean gh-pages gh-pages-push

clean:
	$(RM) $(elc)



#####################################
# Testing

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

gh_pages_branch := gh-pages
gh_pages_base_branch := master
gh_pages_remote := origin

gh-pages-push: gh-pages
	$(git) push $(gh_pages_remote) $(gh_pages_branch)

gh-pages:
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
