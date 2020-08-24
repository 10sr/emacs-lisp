Fuzzy finder app integration into Emacs.

`fuzzy-finder' command opens a new window, starts a fuzzy finder
process inside of it, and then calls a function with selected items.
By default it visits selected files.

You can customize default values used for `fuzzy-finder' execution:
fuzzy finder command, input command, action function and so on.
These values can also be given when calling `fuzzy-finder' as a function,
which is useful when you want to define new interactive commands that uses
`fuzzy-finder'.
