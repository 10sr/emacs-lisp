[![Build Status](https://travis-ci.org/10sr/emacs-lisp.svg?branch=master)](https://travis-ci.org/10sr/emacs-lisp)



Emacs lisp
==========

Some small Emacs lisp libraries.



Public Repository
-----------------

Some packages in this repository can be installed with `package.el`.
To enable this repository, add lines to your init file:

      (setq package-archives
            `(,@package-archives
              ("10sr-el" . "http://10sr.github.io/emacs-lisp/p/")))



### Updating ###

Issue

    make gh-pages

to build packages from current el files and commit them to `gh-pages` branch.
This command does not need the work tree be clean, but it is highly recommended.


If you want only to build packages without committing them to `gh-pages`, issue:

    make archive-all

This puts built packages into `packages/` directory.
