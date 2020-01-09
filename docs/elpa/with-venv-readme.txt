Execute BODY with Python virtual environment activated with `with-venv-dir' macro:

(with-venv-dir (expand-file-name ".venv" default-directory)
  (executable-find "python"))


Alternatively, make this package try to find venv directory automatically
with `with-venv':

(with-venv
  (executable-find "python"))


This macro uses `with-venv-find-venv-dir-functions' to find suitable venv
directory: this function currently support pipenv, poetry, and can find
directories named ".venv".
Or, you can set buffer-local vairable `with-venv-venv-dir' to explicitly
specify path to venv directory.


If you want to always enable `with-venv' for certain functions,
`with-venv-advice-add' can be used for this purpose:

(with-venv-advice-add 'blacken-buffer)

Adviced functions are always wrapped with `with-venv' macro when called.

Call `with-venv-advice-remove' to remove these advices.
