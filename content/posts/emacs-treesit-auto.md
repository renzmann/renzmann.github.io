---
title: "Getting Emacs 29 to Automatically Use Tree-sitter Modes"
author: ["Robb"]
date: 2023-01-22T00:00:00-05:00
lastmod: 2023-01-23T16:09:02-05:00
categories: ["emacs"]
draft: false
weight: 2001
---

Recently, [/u/casouri](https://www.reddit.com/user/casouri/) posted a guide to getting started with the new built-in
tree-sitter capabilities for Emacs 29.  [In that post](https://archive.casouri.cc/note/2023/tree-sitter-in-emacs-29/index.html), they mention that there
will be no automatic major-mode fallback for Emacs 29.  That means I would have
to use `M-x python-ts-mode` manually, or change the entry in `auto-mode-alist` to
use `python-ts-mode`, in order to take advantage of the new tree-sitter
functionality.  Of course, that would still leave the problem of when the Python
tree-sitter grammar isn't installed, in which case `python-ts-mode` is going to
fail.

To solve this issue, I wrote a very small package that adjusts the new
`major-mode-remap-alist` variable based on what grammars are ready on your
machine.  If a language's tree-sitter grammar is installed, it will use that
mode.  If not, it will use the original major mode.  Simple as that!


## For the impatient: `treesit-auto.el` {#for-the-impatient-treesit-auto-dot-el}

The package I wound up with is available on GitHub as [treesit-auto.el](https://github.com/renzmann/treesit-auto).  Using
`package-vc-install`, you can get it right away, and just `use-package` or `require`
to load it:

```text
M-x package-vc-install RET https://github.com/renzmann/treesit-auto.el
```

Then, in your configuration file:

```emacs-lisp
(use-package treesit-auto
  :demand t
  :config
  (treesit-auto-apply-remap))
```

Once I learn how ELPA and MELPA work, I'll try to publish this package there, too.


## How the `treesit-auto.el` works {#how-the-treesit-auto-dot-el-works}

The recommendation in Yuan's article was to use `define-derived-mode` along with
`treesit-ready-p`.  In the NEWS (`C-h n`), however, I noticed a new variable
`major-mode-remap-alist`, which at a glance appears suitable for a similar cause.
For my Emacs configuration, I had two things I wanted to accomplish:

1.  Set all of the URLs for `treesit-language-source-alist` up front, so that I
    need only use `treesit-install-language-grammar RET python RET`, instead of
    writing out everything interactively
2.  Use the same list of available grammars to remap between tree-sitter modes
    and their default fallbacks

Initially, I tried Yuan's suggested approach with `define-derived-mode`, but I
didn't want to repeat code for every major mode I wanted fallback for.  Trying
to expand the major mode names correctly in a loop wound up unwieldy, because
expanding the names properly for the `define-derived-mode` macro was too
challenging for my current skill level with Emacs lisp, and wound up cluttering
the global namespace more than I liked when auto-completing through `M-x`.
Instead, I decided take a two step approach:

1.  Set up `treesit-language-source-alist` with the grammars I'll probably use
2.  Loop over the keys in this alist to define the association between a
    tree-sitter mode and its default fallback through `major-mode-remap-alist`

This makes the code we need to actually write a little simpler, since an
association like `python-mode` to `python-ts-mode` can be automatic (since they
share a name), and we can use a customizable alist for specifying the edge
cases, such as `toml-ts-mode` falling back to `conf-toml-mode`.

To start with, I just had this:

```emacs-lisp
(setq treesit-language-source-alist
  '((bash "https://github.com/tree-sitter/tree-sitter-bash")
    (c "https://github.com/tree-sitter/tree-sitter-c")
    (cmake "https://github.com/uyha/tree-sitter-cmake")
    (common-lisp "https://github.com/theHamsta/tree-sitter-commonlisp")
    (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
    (css "https://github.com/tree-sitter/tree-sitter-css")
    (csharp "https://github.com/tree-sitter/tree-sitter-c-sharp")
    (elisp "https://github.com/Wilfred/tree-sitter-elisp")
    (go "https://github.com/tree-sitter/tree-sitter-go")
    (go-mod "https://github.com/camdencheek/tree-sitter-go-mod")
    (html "https://github.com/tree-sitter/tree-sitter-html")
    (js . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json "https://github.com/tree-sitter/tree-sitter-json")
    (lua "https://github.com/Azganoth/tree-sitter-lua")
    (make "https://github.com/alemuller/tree-sitter-make")
    (markdown "https://github.com/ikatyang/tree-sitter-markdown")
    (python "https://github.com/tree-sitter/tree-sitter-python")
    (r "https://github.com/r-lib/tree-sitter-r")
    (rust "https://github.com/tree-sitter/tree-sitter-rust")
    (toml "https://github.com/tree-sitter/tree-sitter-toml")
    (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
```

At this point, I can just use `M-x treesit-install-language-grammar RET bash` to
get the Bash grammar, and similarly for other languages.

Then, I made an alist of the "weird" cases:

```emacs-lisp
(setq treesit-auto-fallback-alist
      '((toml-ts-mode . conf-toml-mode)
        ;; I don't actually know if the future tree-sitter mode for HTML will be
        ;; called html-ts-mode or mhtml-ts-mode, but if it's the former I'd include this
        (html-ts-mode . mhtml-mode)
        ;; See the note in their README: https://github.com/emacs-typescript/typescript.el#a-short-note-on-development-halt
        (typescript-ts-mode . nil)
        (tsx-ts-mode . nil)))
```

Setting the CDR to `nil` explicitly means I didn't want any type of fallback to be
attempted whatsoever for a given tree-sitter mode, even if something similarly
named might be installed.

Finally, I had a simple loop where I constructed the symbols for the mode and
the tree-sitter mode via `intern` and `concat`, and check whether the tree-sitter
version is available through `treesit-ready-p`.  If it is, we remap the base mode
to the tree-sitter one in `major-mode-remap-alist`.  If it _isn't_ ready, then we do
the opposite: remap the tree-sitter mode to the base version.

```emacs-lisp
(dolist (language-source treesit-language-source-alist)
  (let* ((name (car language-source))
         (name-ts-mode (intern (concat (symbol-name name) "-ts-mode")))
         (fallback-assoc (assq name-ts-mode treesit-auto-fallback-alist))
         (fallback-name (cdr fallback-assoc))
         (name-mode (or fallback-name
                        (intern (concat (symbol-name name) "-mode"))))
         (name-mode-bound-p (fboundp name-mode))
         (skip-remap-p (and fallback-assoc
                            (not (cdr fallback-assoc)))))
    (and (not skip-remap-p)
         (fboundp name-ts-mode)
         (if (treesit-ready-p name t)
             (add-to-list 'major-mode-remap-alist `(,name-mode . ,name-ts-mode))
           (when name-mode-bound-p
             (add-to-list 'major-mode-remap-alist `(,name-ts-mode . ,name-mode)))))))
```

Of course, the [actual code](https://github.com/renzmann/treesit-auto/blob/d3fc07db6d646bee5631bdd28f6e82d2e0690d6d/treesit-auto.el#L96-L110) has a bit more wrapped around it, but the core idea
is more or less the same.
