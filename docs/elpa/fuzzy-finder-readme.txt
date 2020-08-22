Fuzzy finder app integration into Emacs .

`fuzzy-finder' command will open a new window and start a fuzzy finder
process inside of it, and then call a function with selected items.
By default the function is set to the function that visits given
files. so you can use `fuzzy-finder' command as a `find-file'
for existing files.

You can customize default values used for `fuzzy-finder' execution including
fuzzy finder command, input command, action function and so on.
These values can also be given when calling `fuzzy-finder' as a function,
which is useful when you want to define new interactive commands that uses
`fuzzy-finder'.
