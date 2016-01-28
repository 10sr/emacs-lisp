Save buffers automatically when Emacs is idle for spedified seconds.

Idle timer for autosave can be set by sexps like:
(and (require 'autosave nil t)
     (autosave-set 2))
