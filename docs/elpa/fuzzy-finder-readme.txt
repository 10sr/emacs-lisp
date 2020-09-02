Fuzzy finder app integration into Emacs.

`fuzzy-finder' command starts a fuzzy finder process and calls a function
on the selected items.
By default it visits selected files.

There are a number of applications which can be used with `fuzzy-finder'
such as fzf (default), peco, and selecta.

You can customize default values used for `fuzzy-finder' execution:
fuzzy finder command, input command, action function and so on.
These values can also be given when calling `fuzzy-finder' as a function,
which is useful when you want to define new interactive commands that uses
`fuzzy-finder'.
