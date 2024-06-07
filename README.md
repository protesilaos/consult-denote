# `consult-denote` for GNU Emacs

[ NOTE 2024-06-07: The first stable version of this package will be
  released together with Denote version 3.0.0. Probably before the end
  of June 2024. ]

* * *

Integrate the `denote` and `consult` packages:

- [Denote](https://github.com/protesilaos/denote) : A file-naming
  scheme to easily retrieve files of any type. Useful for note-taking
  and long-term storage files.
- [Consult](https://github.com/minad/consult): Enhanced interactivity
  for the standard Emacs minibuffer, such as a preview mechanism for
  buffers and an asynchronous grep/find.

The purpose of `consult-denote` is as follows:

1. **Upgrade all the minibuffer prompts of Denote:** For the time
   being, this means that we show a preview of the file to-be-linked
   or to-be-opened. Simply enable the `consult-denote-mode`. The
   prompts use the same patterns of interaction as core Denote and
   *will never deviate from this paradigm*, such as to prettify titles
   or whatnot (that is an expensive operation that slows down Emacs).

2. **Easy search for the `denote-directory`:** Implement
   Consult-powered Grep and Find commands which operate on the
   `denote-directory` regardless of where they are called from. See
   the commands `consult-denote-grep` and `consult-denote-find`.
   Customise which command they call by modifying the user options
   `consult-denote-grep-command` and `consult-denote-find-command`.

3. **Include Denote "sources" for `consult-buffer`:** This is also
   part of the `consult-denote-mode`. It adds new headings/groups to
   the interface of the `consult-buffer` command. Those lists (i) the
   buffers that visit Denote files, (ii) the subdirectories of the
   `denote-directory`, and (iii) the silos listed in the value of the
   user option `denote-silo-extras-directories` (for those who opt in
   to that extension).

In the future we may use other features of Consult, based on user
feedback.
