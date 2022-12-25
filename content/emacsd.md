---
title: "My Literate .emacs.d"
author: ["Robb Enzmann"]
draft: false
---

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [Goals](#goals)
- [Tangling](#tangling)
- [Inspirations](#inspirations)
- [Header](#header)
- [Custom](#custom)
- [Packages](#packages)
- [OS-specific Configuration](#os-specific-configuration)
    - [Microsoft Windows](#microsoft-windows)
    - [macOS](#macos)
    - [Linux](#linux)
- [Theme: `ef-themes`](#theme-ef-themes)
    - [Gave up on Nord](#gave-up-on-nord)
    - [Messed up colors in TTY mode](#messed-up-colors-in-tty-mode)
- [Emacs' Built-in Settings](#emacs-built-in-settings)
    - [`dabbrev`: swap `M-/` and `C-M-/`](#dabbrev-swap-m-and-c-m)
    - [Mode line](#mode-line)
    - [`eldoc`](#eldoc)
    - [Remember minibuffer history](#remember-minibuffer-history)
    - [Colored output in `eshell`](#colored-output-in-eshell)
    - [Recent files menu](#recent-files-menu)
    - [Fill-column](#fill-column)
    - [Scroll bar](#scroll-bar)
    - [Inhibit splash screen](#inhibit-splash-screen)
    - [Window margins and fringe](#window-margins-and-fringe)
    - [Automatically visit symlink sources](#automatically-visit-symlink-sources)
    - [Indent with spaces by default](#indent-with-spaces-by-default)
    - [Render ASCII color escape codes](#render-ascii-color-escape-codes)
    - [Enable horizontal scrolling with mouse](#enable-horizontal-scrolling-with-mouse)
    - [Window management](#window-management)
    - [Automatically update buffers when contents change on disk](#automatically-update-buffers-when-contents-change-on-disk)
    - [Highlight the line point is on](#highlight-the-line-point-is-on)
    - [Stop stupid bell](#stop-stupid-bell)
    - [Enable split-window dired copying](#enable-split-window-dired-copying)
    - [Automatically create matching parens in programming modes](#automatically-create-matching-parens-in-programming-modes)
    - [Delete whitespace on save](#delete-whitespace-on-save)
    - [Don't wrap lines](#don-t-wrap-lines)
    - [Relative line numbers](#relative-line-numbers)
    - [Delete region when we yank on top of it](#delete-region-when-we-yank-on-top-of-it)
    - [Enable mouse in terminal/TTY](#enable-mouse-in-terminal-tty)
    - [Compilation](#compilation)
    - [Tool bar](#tool-bar)
    - [Ignore risky .dir-locals.el](#ignore-risky-dot-dir-locals-dot-el)
    - [Prefer `rg` and `fd` over `grep` and `find`](#prefer-rg-and-fd-over-grep-and-find)
    - [Make `dired` human-readable](#make-dired-human-readable)
    - [Confirm when exiting Emacs](#confirm-when-exiting-emacs)
    - [Smooth scrolling](#smooth-scrolling)
    - [Prefer `aspell` over `ispell`](#prefer-aspell-over-ispell)
    - [Backup and auto-save files](#backup-and-auto-save-files)
    - [Enable `narrow-to-region`](#enable-narrow-to-region)
    - [Enable up/downcase-region](#enable-up-downcase-region)
- [Keybindings](#keybindings)
    - [Keybound functions](#keybound-functions)
    - [Expanded/better defaults](#expanded-better-defaults)
    - [C-c bindings](#c-c-bindings)
    - [Meta/Alt Modifications](#meta-alt-modifications)
    - [F5-F9](#f5-f9)
    - [Super bindings](#super-bindings)
- [Consulting `completing-read`](#consulting-completing-read)
- [Autocompletion](#autocompletion)
    - [Completion style: Orderless](#completion-style-orderless)
    - [Nicer display of `*Completions*`](#nicer-display-of-completions)
    - [Keybindings to interact with `*Completions*`](#keybindings-to-interact-with-completions)
    - [Minibuffer completion with `vertico` and `marginalia`](#minibuffer-completion-with-vertico-and-marginalia)
    - [Completion at point with `corfu`](#completion-at-point-with-corfu)
- [Tramp](#tramp)
- [TreeSitter](#treesitter)
- [Language-specific major modes](#language-specific-major-modes)
    - [Org-mode](#org-mode)
    - [SQL](#sql)
    - [Python](#python)
    - [Markdown](#markdown)
    - [AutoHotkey](#autohotkey)
    - [csv-mode](#csv-mode)
- [Small tool configuration](#small-tool-configuration)
    - [Embark](#embark)
    - [Coterm mode](#coterm-mode)
    - [Multiple cursors](#multiple-cursors)
    - [Visual fill column](#visual-fill-column)
    - [Magit](#magit)
    - [Change or copy inner/outer](#change-or-copy-inner-outer)
    - [eww - search engine and browser](#eww-search-engine-and-browser)
    - [diff-hl](#diff-hl)
    - [GNU Plot](#gnu-plot)
    - [change-inner](#change-inner)
    - [Esup: startup time profiling](#esup-startup-time-profiling)
    - [Reloading Emacs](#reloading-emacs)
    - [Language Server Protocol (LSP) with `eglot`](#language-server-protocol--lsp--with-eglot)
- [Start a server for `emacsclient`](#start-a-server-for-emacsclient)
- [Don't forget about these](#don-t-forget-about-these)
- [Footer](#footer)

</div>
<!--endtoc-->

Want to use it? Go ahead!

```shell
git clone --depth 1 https://github.com/renzmann/.emacs.d ~/.emacs.d
```

All external dependency sources are explicitly included under the `elpa/`
directory, meaning it's as simple as "clone-n-go".  Opening this document under my
configuration looks like so:

{{< figure src="https://user-images.githubusercontent.com/32076780/201825125-aa94bfa3-0d1a-47d4-b451-be3718850da2.jpg" width="800px" >}}

If you prefer a prettier reading experience, check out this same document weaved
into [my website.](https://robbmann.io/emacsd/)


## Goals {#goals}

I use all three of the major platforms, in both GUI and TTY mode.  So
this config is designed to work equally well for:

| platform | terminal | GUI | ssh + TTY | Tramp |
|----------|----------|-----|-----------|-------|
| Linux    | ✅       | ✅  | ✅        | ✅    |
| macOS    | ✅       | ✅  | ✅        | ✅    |
| Windows  | ❌       | ✅  | ❌        | ✅    |

Once I figure out how to get colors working in Windows terminal, I'll
update this table.  For now though, the 16 bit colors don't react to
my color theme, and so on Windows I rely singularly on GUI mode,
rather than WSL or emacs inside Alacritty.

If I had to sum up the theme of my configuration, it would be "vanilla extract".
In only a few instances do I change overt behavior of Emacs, the most noticiable
departures of course being the color theme, [vertico](https://github.com/minad/vertico) minibuffer completion, and
[corfu](https://github.com/minad/corfu) completion-at-point.  Even with those, though, I want a configuration that
fits my hands in such a way that I remain comfortable using `emacs -Q` with very
little disruption to my normal muscle memory and workflow.

I do make changes to things that I feel "should have been included."  Some
examples of this would be error message support for `pyright` in a `*Compilation*`
buffer, reasonable indentation behavior for SQL files, updating buffers
automatically if their contents change on disk, and syntax highlighting for
source blocks in Markdown.  You may notice that despite the laudable goal of
intentded minimalism, this document is is still quite long, as I have found many
(ever increasing) quirky behaviors of Emacs that I tweak.

Most of my time is spent in Org, SQL, Python, and Bash, so the majority of
configuration lies around these sections.


## Tangling {#tangling}

My configuration is a single literate programming document, which is tangled
into the standard `init.el` and supporting files.  This is so I can keep track of
all the crazy things I try and explain them inline with the final code I decide
to include.  Some platforms like GitHub can render this document in a limited
way, but to see all the final configuration values I use you will likely have to
view this document in Emacs itself.

Why use a literate document for my configuration?  Basically, as I added more
comments and reminders about what some line of code was doing, where I got it
from, and why it might be commented out, the prose grew longer than the actual
code, and so a change of medium felt prudent.  In my case, that's the venerable
[Org mode](https://orgmode.org/), which comes with Emacs and serves as a way to seamlessly weave
commentary and code together.


## Inspirations {#inspirations}

Here's where I put the typical quote about standing on one form of shoulders or
another.  I steal quite a lot from other, more qualified Emacs community
contributors, such as:

-   [Protesilaos Stavrou](https://protesilaos.com/)
-   [Ramón Panadestein](https://panadestein.github.io/emacsd/)
-   [Mickey Petersen](https://www.masteringemacs.org/)
-   [Daniel Mendler](https://github.com/minad)
-   [Omar Antolín Camarena](https://github.com/oantolin)


## Header {#header}

To comply with the Emacs [conventions for libraries](https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html), the tangled init.el must
have the following header and [footer:](#footer)

```emacs-lisp
;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann <robbenzmann@gmail.com>
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration

;;; Code:
```


## Custom {#custom}

I prefer having custom modify its own file.  This next snippet ensures any
`package-install` or `custom` edits go to `custom.el`.

```emacs-lisp
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
```


## Packages {#packages}

The initial cornerstone of every Emacs configuration is a decision on package
management and configuration.  I opt for `use-package` and `package.el`, since both
are built-in to Emacs 29+, which helps maximize stability and portability.  I do
not use the `:ensure t` keyword to install packages.  Instead, I rely on `M-x
package-install` and `M-x package-delete` to manage installations, and allow
`use-package` only to handle the configuration and loading of packages.  As
mentioned in the introduction, each package's source is explicitly included into
version control of my configuration, so I don't worry too much about pinning
package versions in this file.  When I want to update a package, I use `M-x
package-update`, the `package.el` user interface, or delete the package's source
folder and restart emacs.  Should something go wrong, I roll back to a previous
commit.  So far, this method has been reliable for keeping my `init.el` (this
README), `custom.el`, the `package-selected-packages` variable, and `elpa/` directory
all in sync with one another.

```emacs-lisp
(eval-when-compile
  (package-autoremove)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-install-selected-packages))
```

There are also a few hand-made packages I keep around in a special
`.emacs.d/site-lisp` directory.

```emacs-lisp
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
```


## OS-specific Configuration {#os-specific-configuration}


### Microsoft Windows {#microsoft-windows}

Windows, funnily enough, has some trouble registering the Windows key as a
usable modifier for Emacs.  In fact, `s-l` will _never_ be an option, since it's
handled at the hardware level.  I also add a few nice-to-haves, like setting the
default shell to `pwsh` and explicitly pathing out `aspell`, which I always install
with `msys64`.

```emacs-lisp
(when (memq system-type '(windows-nt cygwin ms-dos))
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")
  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "aspell.exe")
  ;; Set default shell to pwsh
  ;; (setq explicit-shell-file-name "pwsh")
  ;; Enable use of Winkey as super
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  ;; If we want to use a hotkey, we have to also register each
  ;; combination specifically, like this:
  (w32-register-hot-key [s-a])
  (w32-register-hot-key [s-b])
  (w32-register-hot-key [s-c])
  (w32-register-hot-key [s-d])
  (w32-register-hot-key [s-e])
  (w32-register-hot-key [s-f])
  (w32-register-hot-key [s-g])
  (w32-register-hot-key [s-h])
  (w32-register-hot-key [s-i])
  (w32-register-hot-key [s-j])
  (w32-register-hot-key [s-k])
  ;; s-l can NEVER be registered as a key combination, since Windows
  ;; handles it at a much lower level.
  ;; (w32-register-hot-key [s-l])
  (w32-register-hot-key [s-m])
  (w32-register-hot-key [s-n])
  (w32-register-hot-key [s-o])
  (w32-register-hot-key [s-p])
  (w32-register-hot-key [s-q])
  (w32-register-hot-key [s-r])
  (w32-register-hot-key [s-s])
  (w32-register-hot-key [s-t])
  (w32-register-hot-key [s-u])
  (w32-register-hot-key [s-v])
  (w32-register-hot-key [s-w])
  (w32-register-hot-key [s-x])
  (w32-register-hot-key [s-y])
  (w32-register-hot-key [s-z]))
```


### macOS {#macos}

Launching Emacs from the typical application launcher or command-space usually won't capture any modifications to `$PATH`, typically handled in a file like `~/.profile` or `~/.bashrc`. So, the main configuration included here is from [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

```emacs-lisp
(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (exec-path-from-shell-initialize))
```


### Linux {#linux}

Very little to do here.  Emacs on Linux seems to "just work".

```emacs-lisp
(when (eq system-type 'gnu/linux)
  ;; (set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")
  )
```


## Theme: `ef-themes` {#theme-ef-themes}

[Prot's](https://protesilaos.com/) themes have been reliably legible in nearly every situation.  Now with
his new [ef-themes](https://protesilaos.com/emacs/ef-themes), they're pretty, too! The `ef-themes-headings` variable creates
larger, bolder headings when in [Org-mode](#org-mode), and `ef-themes-to-toggle` allows me to
quickly switch between preset light and dark themes depending on the ambient
light of the room I'm in.

```emacs-lisp
(use-package ef-themes
  :demand t
  :bind ("C-c m" . ef-themes-toggle)

  :init
  (setq ef-themes-headings
        '((0 . (1.9))
          (1 . (1.8))
          (2 . (1.7))
          (3 . (1.6))
          (4 . (1.5))
          (5 . (1.4)) ; absence of weight means `bold'
          (6 . (1.3))
          (7 . (1.2))
          (t . (1.1))))
  (setq ef-themes-to-toggle '(ef-cherie ef-summer))

  :config
  (load-theme 'ef-cherie :no-confirm))
```

I LOVE these themes from `ef-themes`:

-   **Light**
    -   `ef-frost`
    -   `ef-light`
    -   `ef-summer`
-   **Dark**
    -   `ef-cherie`
    -   `ef-trio-dark`
    -   `ef-winter`

I've mostly settled on `ef-cherie`, but sometimes switch to the others above.


### Gave up on Nord {#gave-up-on-nord}

It's worth mentioning that I've tried [nord-theme](https://www.nordtheme.com/ports/emacs/) a couple times and found that
the legibility or contrast wasn't quite good enough in some modes.  Though I
still employ Nord for my terminal config in Alacritty and Kitty, where it looks
_excellent_.  I also still actively use the [nordfox](https://github.com/EdenEast/nightfox.nvim) theme in Neovim, which sports
a beautiful TreeSitter integration.


### Messed up colors in TTY mode {#messed-up-colors-in-tty-mode}

In TTY mode, I use [kitty](https://sw.kovidgoyal.net/kitty/).  I have had trouble with dark blue or red themes in
Alacritty, and on Windows terminal.  There is probably some hacking I could do
on my `$TERM` variable to try and sort that out, but since it just kinda works in
Kitty for me, I haven't spent too much time looking into it.


## Emacs' Built-in Settings {#emacs-built-in-settings}

My settings for base Emacs.  Assuming I ran with _no_ plugins (ala `emacs -Q`), I
would still set most of these by hand at one point or another.


### `dabbrev`: swap `M-/` and `C-M-/` {#dabbrev-swap-m-and-c-m}

```emacs-lisp
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
```


### Mode line {#mode-line}

It's easy for the mode line to get cluttered once things like Flymake and eglot
kick in.  When I was starting out, I used to have these two settings:

```emacs-lisp
(setq display-battery-mode t
      display-time-day-and-date t)

(display-time)
```

After a while I noticed that I'm almost never running Emacs in a full screen
where I can't see the battery or date in the corner of my window manager, so
they were just wasting mode line space.  Nowadays I simply opt for column mode
and a dimmed mode line in non-selected windows.

```emacs-lisp
(setq column-number-mode t
      mode-line-in-non-selected-windows t)
```


### `eldoc` {#eldoc}

I find it very distracting when `eldoc` suddenly pops up and consumes a large part
of the screen for docstrings in python.

```emacs-lisp
(setq eldoc-echo-area-use-multiline-p nil)
```


### Remember minibuffer history {#remember-minibuffer-history}

Found this on a [System Crafters video](https://www.youtube.com/watch?v=51eSeqcaikM).

```emacs-lisp
(setq history-length 25)
(savehist-mode 1)
```


### Colored output in `eshell` {#colored-output-in-eshell}

Copy-pasted from a [stack overflow question](https://emacs.stackexchange.com/questions/9517/colored-git-output-in-eshell).

```emacs-lisp
(add-hook 'eshell-preoutput-filter-functions  'ansi-color-apply)
```


### Recent files menu {#recent-files-menu}

This enables "File -&gt; Open Recent" from the menu bar, and `consult-recent-file`.

```emacs-lisp
(recentf-mode t)
```


### Fill-column {#fill-column}

Regardless of whether we're doing visual fill or hard fill, I like the default
at around 80 characters, and I'll manually change it per buffer if I want
something different

```emacs-lisp
(setq-default fill-column 80)
```


### Scroll bar {#scroll-bar}

I toggle this one on/off sometimes depending on how I feel and which OS I'm
currently on.

```emacs-lisp
(scroll-bar-mode -1)
```


### Inhibit splash screen {#inhibit-splash-screen}

When I was starting out, the OG GNU Splash was a _super_ helpful screen.  Lately I
haven't needed the reminder links as often, but I sometimes get back to it to
with `C-h C-a`.

```emacs-lisp
(setq inhibit-splash-screen t)
```


### Window margins and fringe {#window-margins-and-fringe}

This hunk adds some space around all sides of each window so that we get a clear
space between the edge of the screen and the fringe.  This helps `src` blocks look
clean and well delineated for [org-modern](#org-modern).

```emacs-lisp
(modify-all-frames-parameters
 '((right-divider-width . 40)
   (internal-border-width . 40)))

(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))

(set-face-background 'fringe (face-attribute 'default :background))
```


### Automatically visit symlink sources {#automatically-visit-symlink-sources}

When navigating to a file that is a symlink, this automatically redirects us to
the source file it's pointing to.

```emacs-lisp
(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
```


### Indent with spaces by default {#indent-with-spaces-by-default}

For the most part I edit Python, SQL, Markdown, Org, and shell scripts.  All of
these favor spaces over tabs, so I prefer this as the default.

```emacs-lisp
(setq-default indent-tabs-mode nil)
```


### Render ASCII color escape codes {#render-ascii-color-escape-codes}

For files containing color escape codes, this provides a way to render the
colors in-buffer.

```emacs-lisp
(defun renz/display-ansi-colors ()
  (interactive)
  (require 'ansi-color)
  (ansi-color-apply-on-region (point-min) (point-max)))
```


### Enable horizontal scrolling with mouse {#enable-horizontal-scrolling-with-mouse}

From a helpful [stackoverflow answer.](https://stackoverflow.com/a/67758169)

```emacs-lisp
(setq mouse-wheel-tilt-scroll t)
```


### Window management {#window-management}

From a Mickey Petersen [article](https://www.masteringemacs.org/article/demystifying-emacs-window-manager), this causes `switch-to-buffer` to open the
selected buffer in the current window rather than switching windows, assuming
both are open in the current frame.  This is more frequently the behavior I
intend when I'm trying to get a window to display a specific buffer.

```emacs-lisp
(unless (version< emacs-version "27.1")
  (setq switch-to-buffer-obey-display-actions t))
```


### Automatically update buffers when contents change on disk {#automatically-update-buffers-when-contents-change-on-disk}

Without setting `global-auto-revert-mode`, we have to remember to issue a
`revert-buffer` or `revert-buffer-quick` (`C-x x g` by default) in case a file
changed.  Over Tramp, we still have to manually revert files when they've
changed on disk.

```emacs-lisp
(global-auto-revert-mode)
```


### Highlight the line point is on {#highlight-the-line-point-is-on}

Add a faint background highlight to the line we're editing.

```emacs-lisp
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
```


### Stop stupid bell {#stop-stupid-bell}

This snippet has a special place in my heart, because it was the first two lines
of elisp I wrote when first learning Emacs.

```emacs-lisp
;; Stop stupid bell
(setq ring-bell-function 'ignore)
```

The bell is really, _really_ annoying.


### Enable split-window dired copying {#enable-split-window-dired-copying}

Do-What-I-Mean (DWIM) copying is for when two dired windows are open, and we
want to copy something from one location to the other.  By enabling
`dired-dwim-target`, it auto-populates the minibuffer with the other dired
window's path when issuing a copy command with `C`.

```emacs-lisp
(setq dired-dwim-target t)
```


### Automatically create matching parens in programming modes {#automatically-create-matching-parens-in-programming-modes}

```emacs-lisp
(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
```


### Delete whitespace on save {#delete-whitespace-on-save}

I would also like to have a good-looking display for trailing whitespace and
leading tabs like in my Neovim setup, but it has proven challenging to just
narrow down to those two faces.  In the interim, I toggle `M-x whitespace-mode` to
check for mixed tabs, spaces, and line endings.

```emacs-lisp
(add-hook 'before-save-hook 'delete-trailing-whitespace)
```


### Don't wrap lines {#don-t-wrap-lines}

```emacs-lisp
(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (toggle-truncate-lines nil))
```


### Relative line numbers {#relative-line-numbers}

For programming and prose/writing modes.

Unfortunately, line numbers are displayed in the text area of the buffer, but
org-modern uses the fringe to display source blocks.  [There's no way to display
them to the left](https://www.reddit.com/r/emacs/comments/ymprwi/comment/iv5iafb/?utm_source=share&utm_medium=web2x&context=3) of the fringe, so I'm careful about only turning on line
numbers in modes that I think I'll benefit from it.  It's been working pretty
well in org-mode without the line numbers so far, since for each of the code
blocks I can always use `C-c '` to edit in `prog-mode`, where I _do_ get line numbers.

```emacs-lisp
(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(add-hook 'yaml-mode-hook (lambda () (setq display-line-numbers 'relative)))
(unless (display-graphic-p)
  (add-hook 'text-mode-hook (lambda () (setq display-line-numbers 'relative))))
```


### Delete region when we yank on top of it {#delete-region-when-we-yank-on-top-of-it}

I just think that's a funny sentence.  Normally when yanking text with an active
region, the region will remain and the yanked text is just inserted at point.  I
prefer the modern word processor behavior of replacing the selected text with
the yanked content.

```emacs-lisp
(delete-selection-mode t)
```


### Enable mouse in terminal/TTY {#enable-mouse-in-terminal-tty}

```emacs-lisp
(xterm-mouse-mode 1)
```


### Compilation {#compilation}

As new text appears, the default behavior is for it to spill off the bottom
where we can't see it.  Instead, I prefer the window to scroll along with text
as it appears

```emacs-lisp
(setq compilation-scroll-output t)
```

Enable colors in the `*compilation*` buffer.  Provided by a [helpful stackoverflow
answer](https://stackoverflow.com/a/3072831/13215205).

```emacs-lisp
(defun renz/colorize-compilation-buffer ()
  "Enable colors in the *compilation* buffer."
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'renz/colorize-compilation-buffer)
```


### Tool bar {#tool-bar}

I usually leave the tool bar disabled

```emacs-lisp
(tool-bar-mode -1)
```

The _menu_ bar, on the other hand `(menu-bar-mode)`, is very handy, and I don't
think I'll ever disable it.


### Ignore risky .dir-locals.el {#ignore-risky-dot-dir-locals-dot-el}

From an [Emacs stackexchange](https://emacs.stackexchange.com/a/44604) answer.

```emacs-lisp
(advice-add 'risky-local-variable-p :override #'ignore)
```


### Prefer `rg` and `fd` over `grep` and `find` {#prefer-rg-and-fd-over-grep-and-find}

```emacs-lisp
(when (executable-find "rg")
  (setq grep-program "rg"))

(when (executable-find "fd")
  (setq find-program "fd"))
```


### Make `dired` human-readable {#make-dired-human-readable}

By default, `dired` uses bytes instead of "K", "Mb", or "G" for file sizes.

```emacs-lisp
(setq dired-listing-switches "-alFh")
;; (setq-default dired-hide-details-mode t)
```


### Confirm when exiting Emacs {#confirm-when-exiting-emacs}

It's very annoying when I'm working and suddenly I meant to do `C-c C-x`, but
instead hit `C-x C-c`.  This helps prevent that.

```emacs-lisp
(setq confirm-kill-emacs 'yes-or-no-p)
```


### Smooth scrolling {#smooth-scrolling}

Emacs 29 introduced smooth, pixel-level scrolling, which removes much of the
"jumpiness" you see when scrolling past images.

```emacs-lisp
(if (version< emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))
```


### Prefer `aspell` over `ispell` {#prefer-aspell-over-ispell}

```emacs-lisp
(when (executable-find "aspell")
  (setq ispell-program-name "aspell"))
```


### Backup and auto-save files {#backup-and-auto-save-files}

Keep all backup files in a temporary folder.  At the moment I have some "file
not found" errors popping up during auto-save on Windows.  Once I debug that,
I'll uncomment the second part.

```emacs-lisp
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/"))
      ;; auto-save-file-name-transforms
      ;; '(("." ,temporary-file-directory t))
      )
```


### Enable `narrow-to-region` {#enable-narrow-to-region}

```emacs-lisp
(put 'narrow-to-region 'disabled nil)
```


### Enable up/downcase-region {#enable-up-downcase-region}

```emacs-lisp
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
```


## Keybindings {#keybindings}

In the unlikely event that something below is loaded incorrectly and causes
initialization to stop, I like to have my basic key command loaded early,
especially so that navigating to the error can happen more quickly.

Emacs has [some standards](https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html) about where user-configured keys should go.  Other
non-standard modifications are marked as such.


### Keybound functions {#keybound-functions}

Special utility functions that we'll bind to user keys.

```emacs-lisp
(defun renz/--jump-section (dirname prompt extension)
  "For internal use: prompt for a file under `dirname' in the user
emacs config site with matching `extension' regexp"
  (find-file
   (concat dirname
           (completing-read prompt
                            (directory-files dirname nil extension)))))

(setq renz/site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my site-lisp folder, then go there."
  (interactive)
  (renz/--jump-section renz/site-lisp-dir
                       "Elisp config files: "
                       ".*\.el$"))

(defun renz/jump-org ()
  "Prompt for an org file in my emacs directory, then go there."
  (interactive)
  (renz/--jump-section renz/org-home
                       "Org files: "
                       ".*\.org$"))

(defun renz/jump-init ()
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory))
  (consult-org-heading))

(defun renz/find-tag ()
  "Use completing-read to navigate to a tag"
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(defun renz/consult-grep ()
  "Live grep using `rg' if found, otherwise `grep'"
  (interactive)
  (if (executable-find "rg")
      (consult-ripgrep)
    (consult-grep)))
```


### Expanded/better defaults {#expanded-better-defaults}

These convenient chords allow for fast text replacement by holding `C-M-` and
rapidly typing `k` and `h` in succession.

```emacs-lisp
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
```

The next line UNBINDS the suspend-frame keybinding.  Accidentally minimizing on
the GUI was frustrating as hell, so now I use `C-x C-z` if I _really_ want to
suspend the frame.

```emacs-lisp
(global-set-key (kbd "C-z") #'zap-up-to-char)
```

Hippie-expand [is purported](https://www.masteringemacs.org/article/text-expansion-hippie-expand) to be a better version of `dabbrev`, but I rather like
the default behavior of `dabbrev`.  I typically have `hippie-expand` on a dedicated
key, and sometimes re-bind the default `M-/` as well, depending on my current
workflow.

```emacs-lisp
(global-set-key [remap dabbrev-expand] 'hippie-expand)
```

`ibuffer` is a strictly superior, built-in version of its counterpart.

```emacs-lisp
(global-set-key [remap list-buffers] 'ibuffer)
```

The most common situation where I'm running `flymake` would be for spelling in
prose, or diagnostics from a language server.  In either case, I like having
next/previous on easy to reach chords.

```emacs-lisp
(use-package flymake
  :bind (:map flymake-mode-map
         ("M-n" . flymake-goto-next-error)
         ("M-p" . flymake-goto-prev-error)))
```

When using `isearch` to jump to things, it's sometimes convenient to re-position
point on the opposite side of where the search would normally put it.  E.g. when
using `C-r`, but we want point to be at the end of the word when we're done.
Provided by a [stack overflow answer](https://emacs.stackexchange.com/a/52554).

```emacs-lisp
(define-key isearch-mode-map (kbd "<C-return>")
  (defun isearch-done-opposite (&optional nopush edit)
    "End current search in the opposite side of the match."
    (interactive)
    (funcall #'isearch-done nopush edit)
    (when isearch-other-end (goto-char isearch-other-end))))
```


### C-c bindings {#c-c-bindings}

`C-c` &lt;letter&gt; is always free for users.  It may seem like overkill how I set a
header for each possible `C-c` combination, but it's incredibly handy when I want
to jump directly to one of these headings while in another buffer.  See
e.g. `renz/jump-init`, which allows me to narrow in on a particular key I'd like
to bind by leveraging `completing-read`.  If a `C-c <letter>` combination is missing as a header, then I'm probably using it
in a `:bind` statement with `use-package` somewhere else.


#### `C-c b` scroll-bar-mode {#c-c-b-scroll-bar-mode}

```emacs-lisp
;; (global-set-key (kbd "C-c a") #')
(global-set-key (kbd "C-c b") #'scroll-bar-mode)
```


#### `C-c d` jump to a tag {#c-c-d-jump-to-a-tag}

```emacs-lisp
(global-set-key (kbd "C-c d") #'renz/find-tag)
```


#### `C-c e` {#c-c-e}

```emacs-lisp
;; (global-set-key (kbd "C-c e") #')
```


#### `C-c f` hippie-expand {#c-c-f-hippie-expand}

```emacs-lisp
(global-set-key (kbd "C-c f") #'hippie-expand)
```


#### `C-c g` find file at point {#c-c-g-find-file-at-point}

```emacs-lisp
(global-set-key (kbd "C-c g") #'ffap)  ; inspired by vim `gf`
```


#### `C-c i` jump to a header in my configuration {#c-c-i-jump-to-a-header-in-my-configuration}

```emacs-lisp
(global-set-key (kbd "C-c i i") #'renz/jump-init)
(global-set-key (kbd "C-c i l") #'renz/jump-configuration)
```


#### `C-c j` Toggle window split {#c-c-j-toggle-window-split}

[Toggling windows](https://www.emacswiki.org/emacs/ToggleWindowSplit) from vertical to horizontal splits and vice-versa.

```emacs-lisp
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c j") #'toggle-window-split)
```


#### `C-c k` kill all but one space {#c-c-k-kill-all-but-one-space}

```emacs-lisp
(global-set-key (kbd "C-c k") #'just-one-space)
```


#### `C-c q` replace regexp {#c-c-q-replace-regexp}

```emacs-lisp
(global-set-key (kbd "C-c q") #'replace-regexp)
```


#### `C-c s` shell {#c-c-s-shell}

```emacs-lisp
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'term)
```


#### `C-c t` {#c-c-t}

```emacs-lisp
;; (global-set-key (kbd "C-c t") #')
```


#### `C-c u` Consult grep/rg {#c-c-u-consult-grep-rg}

```emacs-lisp
(global-set-key (kbd "C-c u") #'renz/consult-grep)
```


#### `C-c w` {#c-c-w}

```emacs-lisp
(global-set-key (kbd "C-c w") #'whitespace-mode)
```


#### `C-c x` {#c-c-x}

```emacs-lisp
;; (global-set-key (kbd "C-c x") #')
```


#### `C-c z` {#c-c-z}

```emacs-lisp
;; (global-set-key (kbd "C-c z") #')
```


#### `C-c` Other bindings {#c-c-other-bindings}

```emacs-lisp
(global-set-key (kbd "C-c ;") #'comment-line)  ; TTY-friendly
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly
```


### Meta/Alt Modifications {#meta-alt-modifications}

```emacs-lisp
(with-eval-after-load 'dired
(define-key dired-mode-map (kbd "M-S-j") 'dired-goto-file))
```


### F5-F9 {#f5-f9}

Like the `C-c <letter>` bindings, these are reserved for users.  In practice, even
though there are few of these keys, I tend to forget which is which.  So I wind
up using things bound to my `C-c` keymaps instead, since they come from a natural,
nested language.

```emacs-lisp
(global-set-key (kbd "<f5>") #'compile)
(global-set-key (kbd "M-<f5>") #'recompile)
;; (global-set-key (kbd "<f6>") #')
;; (global-set-key (kbd "M-<f6>") #')
;; (global-set-key (kbd "<f7>") #')
;; (global-set-key (kbd "M-<f7>") #')
;; (global-set-key (kbd "<f8>") #')
;; (global-set-key (kbd "M-<f8>") #')
;; (global-set-key (kbd "<f9>") #'vterm)
(global-set-key (kbd "M-<f9>") #'eshell)
(global-set-key (kbd "S-<f9>") #'ansi-term)
(global-set-key (kbd "s-<f9>") #'shell)
```


### Super bindings {#super-bindings}

See the [Microsoft Windows](#microsoft-windows) section for some hackery required to get these working
on their operating system.

```emacs-lisp
(global-set-key (kbd "s-c") #'kill-ring-save)
(global-set-key (kbd "s-q") #'save-buffers-kill-terminal)
(global-set-key (kbd "s-s") #'save-buffer)
(global-set-key (kbd "s-t") #'tab-new)
(global-set-key (kbd "s-v") #'yank)
```


## Consulting `completing-read` {#consulting-completing-read}

[Consult](https://github.com/minad/consult) forms a large foundation of my workflow.  It provides a strictly
superior experience switching between buffers, performing `grep` or `rg` with live
results as you type, and scanning through a document for lines matching an
expression with `consult-line`.

```emacs-lisp
(use-package consult
  :bind(
        ;; C-x bindings (ctl-x-map)
        ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
        ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
        ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
        ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
        ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
        ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer

        ;; Other custom bindings
        ("M-y" . consult-yank-pop)                ;; orig. yank-pop
        ("<help> a" . consult-apropos)            ;; orig. apropos-command
        ("C-c r" . consult-recent-file)

        ;; M-g bindings (goto-map)
        ("M-g e" . consult-compile-error)
        ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
        ("M-g g" . consult-goto-line)             ;; orig. goto-line
        ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
        ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
        ("M-g m" . consult-mark)
        ("M-g k" . consult-global-mark)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-imenu-multi)

        ;; M-s bindings (search-map)
        ("M-s d" . consult-find)
        ("M-s D" . consult-locate)
        ("M-s g" . consult-grep)
        ("M-s G" . consult-git-grep)
        ("M-s r" . consult-ripgrep)
        ("M-s l" . consult-line)
        ("M-s L" . consult-line-multi)
        ("M-s m" . consult-multi-occur)
        ("M-s k" . consult-keep-lines)
        ("M-s u" . consult-focus-lines)

        ;; Isearch integration
        ("M-s e" . consult-isearch-history)
        :map isearch-mode-map
        ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
        ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
        ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
        ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch

        ;; Minibuffer history
        :map minibuffer-local-map
        ("M-s" . consult-history)                 ;; orig. next-matching-history-element
        ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  )
```


## Autocompletion {#autocompletion}

Emacs offers incredible depth and freedom when configuring methods that
automatically complete text.  There are actually two things that
"autocompletion" can refer to in Emacs:

1.  [Minibuffer completion](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html)
2.  [Completion at point](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html)

Emacs on its own does not have a nice pop-up-menu like Vim for completing text
at point.  For both the minibuffer and `completion-at-point` it uses a special
buffer called `*Completions*`, from which we can see (and optionally select) a
completion from potential candidates.  It only updates when issuing a command,
such as hitting TAB, rather than updating live as we type, which makes it a bit
clunky compared to what we see in modern editors.  We will lean on some external
packages to remedy this, but before getting to that we have two orders of
business:

1.  Setting the _completion style_
2.  Enhancing the behavior of the default `*Completions*` buffer and how we can
    interact with it.


### Completion style: Orderless {#completion-style-orderless}

For both the minibuffer and `completion-at-point`, I use the same _completion
style_.  Completion style is the method of assigning completion candidates to a
given input string.  `flex` is the built-in "fuzzy" completion style, familiar to
us from symbol completion in IDEs and VSCode's command palette.  `basic` functions
much like your default TAB-complete at a Bash shell.

```emacs-lisp
(setq completion-styles '(flex basic partial-completion emacs22))
```

I've found the [orderless](https://github.com/oantolin/orderless) completion style especially well-suited to Emacs.  It
allows me to type short strings that can match the symbol I'm looking for in any
order.  In Emacs, I may not know if I'm looking for `package-list` or
`list-packages`.  In either case, I can just type "`pack lis`" in the minibuffer to find
the correct one.

```emacs-lisp
(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
```


### Nicer display of `*Completions*` {#nicer-display-of-completions}

With the _completion style_ set, we have to now configure the interfaces for
_displaying_ candidates as we type.  The natural place to start would be the
built-in `*Completions*` buffer.  First, I want candidates displayed as a single,
vertical list.

```emacs-lisp
(setq completions-format 'one-column)
```

Also, when using the built-in completion-at-point, the `*Completions*` buffer can
sometimes take up the whole screen when there are a lot of candidates.  [Setting
this](https://www.gnu.org/software/emacs/manual/html_node/emacs/Temporary-Displays.html) prevents that:

```emacs-lisp
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 20)
```


#### Live `*Completions*` buffer {#live-completions-buffer}

[An interesting idea by oantolin.](https://github.com/oantolin/live-completions)  Given how modest in scope it is, and how close
to Vanilla Emacs it stays, I might actually try it sometime.


### Keybindings to interact with `*Completions*` {#keybindings-to-interact-with-completions}

If there is a convenient way to interact with the `*Completions*` buffer from a
buffer with default key bindings, I am not aware of it.  Because of this, I set
a few convenience functions for navigating to, selecting, and closing the buffer
in the case I do need to use it.

```emacs-lisp
(defun renz/completion-accept ()
  "Expand current text to first completion result"
  (interactive)
  ;; FIXME In python REPL, if we go back inside a symbol and edit it
  ;;       to narrow the candidate list, then accept something with
  ;;       this function, the trailing text isn't erased
  (switch-to-completions)
  (choose-completion))

(defun renz/jump-completion ()
  "Jump to second completion."
  (interactive)
  (switch-to-completions)
  (next-completion 1))

(defun renz/completion-kill-completion-buffer ()
  "Close the *Completions* buffer without switching to it"
  (interactive)
  (kill-buffer "*Completions*"))
```

Much like Vim's built-in completion with the pop-up menu, I set `C-n` and `C-p` as a
way to select completion candidates out of the `*Completions*` buffer.

```emacs-lisp
(define-key completion-in-region-mode-map (kbd "C-n") 'renz/jump-completion)
(define-key completion-list-mode-map (kbd "C-n") 'next-completion)
(define-key completion-list-mode-map (kbd "C-p") 'previous-completion)
```

For a while, I thought keys like `RET`, `TAB`, and similar would be intuitive
candidates for accepting completion candidates.  That turned out to be a problem
because there's a good chance you'll mess up required functionality in shell,
minibuffer, and related modes.  So, instead, I opt for the similar `C-j`.

```emacs-lisp
(define-key completion-in-region-mode-map (kbd "C-j") 'renz/completion-accept)
(define-key completion-list-mode-map (kbd "C-j") 'choose-completion)
```


### Minibuffer completion with `vertico` and `marginalia` {#minibuffer-completion-with-vertico-and-marginalia}

It's worth noting that the `fido-vertical` built-in is pretty good, but I had
issues with micro-freezes in some situations.  [vertico](https://github.com/minad/vertico), on the other hand, has
been lightning quick, and has intuitive keybindings that don't require any
futzing.  _Especially_ in the case where I'm looking to tab-complete things like
`C-x C-f /ssh:<thing>`.

```emacs-lisp
(use-package vertico
  :config
  (vertico-mode)
  (vertico-buffer-mode -1)
  (define-key vertico-map "\M-q" #'vertico-quick-insert)
  (define-key vertico-map "\C-q" #'vertico-quick-exit)

  (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((consult-grep buffer))))
```

Combining `vertico`'s forces with [marginalia](https://github.com/minad/marginalia) creates a lovely minibuffer
completion experience that rivals (or even beats) modern IDE and VSCode command
palettes.  `marginalia` adds a short, context-aware description next to completion
candidates in the minibuffer.  For instance, using `C-h f` will show me if a
function is already bound to a key, and give me the top-level description of the
function, without requiring me to actually open the `*Help*` buffer.

```emacs-lisp
(use-package marginalia
  :config (marginalia-mode))
```

For some eye candy, I _could_ add some all-the-icons goodies as well.

```emacs-lisp
(use-package all-the-icons-completion
  :if (display-graphic-p)
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))
```


### Completion at point with `corfu` {#completion-at-point-with-corfu}

For `completion-at-=point` suggestions, I like `corfu` a lot.  It's philosophy is to
stick as close as possible to the native Emacs internal API as possible, without
reinventing the wheel.  In my experience, this has meant far fewer integration
troubles with other packages.  It uses child frames for displaying the
completion candidates, however, which means we need a separate `corfu-terminal`
extension for it to work in TTY mode.  While `use-package` has the `:unless` and `:if`
keywords, I seem to have trouble getting them to actually work with
`display-graphic-p`, and the official instructions with `window-system` wasn't
working for me.  Hence, it's wrapped in an `unless` block.

I've also enabled the TNG (Tab-n-go) style of completion, as laid out in corfu's
[README](https://github.com/minad/corfu#tab-and-go-completion).

```emacs-lisp
(unless (display-graphic-p)
  (use-package corfu-terminal
    :config
    (corfu-terminal-mode +1)))

(use-package corfu
  :demand t

  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection

  :bind
  (:map corfu-map
        ("M-SPC" . corfu-insert-separator)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))

  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (setq corfu-auto t
        corfu-auto-delay 0.0
        corfu-quit-no-match 'separator)

  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (advice-add #'corfu-insert :after #'corfu-send-shell)

  (global-corfu-mode))
```

There are some cases over Tramp, however, where corfu will case some performance
issues.  Especially in the case where some folders under the `/` root might be
mounted over a network.  In that case, I sometimes call this
`renz/disable-corfu-remote`, which only disables corfu in the current buffer if
it's being handled by Tramp.

```emacs-lisp
(defun renz/disable-corfu-remote ()
  (when (and (fboundp 'corfu-mode)
             (file-remote-p default-directory))
    (corfu-mode -1)))
```

Finally, Emacs uses `M-TAB`, or the equivalent `C-M-i` for `completion-at-point`.  I'd much
prefer to just use the easier and more intuitive `TAB`.

```emacs-lisp
(setq tab-always-indent 'complete)
```


## Tramp {#tramp}

Tramp (Transparent Remote Access Multiple Protocol) allows us to access files on
a remote machine, and edit them locally.  This is great for simple changes or
quickly testing out some Python on a VM somewhere.  It isn't as snappy as using
the TTY version or an X-forwarded Emacs from the server directly, so if I _can_
set up Emacs remotely, I usually do.  When I don't want to or don't have the
time, Tramp is a godsend.  There are, however, many foibles to guard against,
particularly with how interacts with version control and `.dir-locals`.  The
Tramp manual (distributed with Emacs) recommends adjusting these for some speed
improvements:

```emacs-lisp
(use-package tramp
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1))
```

eglot is [actively working](https://github.com/joaotavora/eglot/issues/859) on an issue related to timers causing a "Forbidden
reentrant call of Tramp" message and freezing.  In the meantime, this setting
was recommended.

```emacs-lisp
(setq tramp-use-ssh-controlmaster-options nil)
```

For some time I was having a lot of trouble with prohibitive slowness over
Tramp, and after careful scrutiny of the logs on (I believe) `tramp-verbose 6`, I
found out that enabling remote dir-locals was causing a huge bottleneck.  On
every operation it would trace up the filesystem tree back to the root
directory, scanning for a `.dir-locals` file.  Since some of the drives were
network-mounted, this caused thousands of network calls per file operation,
obviously slowing things down a lot.  Because of this, I've opted to simply
disable `.dir-locals` over Tramp entirely, since I don't really use it much, if at
all.

```emacs-lisp
;; (setq enable-remote-dir-locals t)
```

When using [conda](https://anaconda.org), I keep a special conda environment named `robbmann` for
locally-installed and managed command line utilities.  Sometimes I link these
over to `.local/bin`, and other times I forget.  For the latter case, I tend to
include it in a lot of my PATH-setting situations.

```emacs-lisp
(add-to-list 'tramp-remote-path "~/.local/bin")
(add-to-list 'tramp-remote-path "~/.conda/envs/robbmann/bin")
```

[Disabling VC](https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html) _does_ seem to speed things up a little, but it's not an acceptable
thing to put in, since I so frequently use VC over tramp.  Fully disabling VC
would include this snippet:

```emacs-lisp
(remove-hook 'find-file-hook 'vc-find-file-hook)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
```

Additionally, these came up as other potential options [from the doom-emacs
issues](https://github.com/doomemacs/doomemacs/issues/3909), which I do not currently include.

```emacs-lisp
(setq tramp-default-method "scp")
(setq projectile--mode-line "Projectile")
```

I often need to set these in ~/.ssh/config for TRAMP to speed up

```text
Host *
     ControlMaster auto
     ControlPath ~/.ssh/master-%h:%p
     ControlPersist 10m
     ForwardAgent yes
     ServerAliveInterval 60
```


## TreeSitter {#treesitter}

Emacs 29 added native [TreeSitter](https://tree-sitter.github.io/tree-sitter/) support.  TreeSitter is a new way of
incrementally parsing source code that offers superior navigation and syntax
highlighting.  To fully realize this benefit, however, it requires that we
install `tree-sitter` grammars independently from Emacs.  Right now, I'm using
[casouri's modules](https://github.com/casouri/tree-sitter-module), which I build and install under `~/.emacs.d/tree-sitter`, if
they don't already exist under `/usr/local/lib/` or `~/.local/lib`.

```shell
git clone git@github.com:casouri/tree-sitter-module.git
cd tree-sitter-module
./batch.sh
mkdir -p ~/.emacs.d/tree-sitter
cp ./dist/* ~/.emacs.d/tree-sitter/
```

In case of the latter, I just add extra paths to `treesit-extra-load-path`
explicitly.

```emacs-lisp
(add-to-list 'treesit-extra-load-path "/usr/local/lib/")
(add-to-list 'treesit-extra-load-path "~/.local/lib/")
```

For the full instructions, the commit history of adding the `tree-sitter` modules
to Emacs included a [full guide](https://git.savannah.gnu.org/cgit/emacs.git/plain/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter), which can be read in Info under "Parsing Program
Source".

```text
C-h i d m elisp RET g Parsing Program Source RET
```

Enabling TreeSitter is done on a per-language basis to override the default
major mode with the corresponding TreeSitter version.


## Language-specific major modes {#language-specific-major-modes}


### Org-mode {#org-mode}

```emacs-lisp
(setq renz/org-home "~/org/")
(setq org-confirm-babel-evaluate nil)
(setq org-edit-src-content-indentation 0)
```

I use `consult-org-heading` for jumping between headers now, so I no longer tangle
this line into my config.

```emacs-lisp
(setq org-goto-interface 'outline-path-completion)
```

When displaying images, I usually like to resize them to a comfortable width,
which the following enables.

```emacs-lisp
(setq org-image-actual-width nil)
```

I often want to kill and paste entire `src` blocks at a time, along with their
results.  For a time, I included the following command:

```emacs-lisp
(defun renz/org-kill-src-block ()
  "Kill the src block around point, if applicable."
  (interactive)
  (org-babel-remove-result)
  (org-mark-element)
  (kill-region nil nil t))
```

However, this won't add the contents of the `#+RESULTS:` block to the kill-ring,
which meant I confused myself often.  Instead, I've found that simply doing `M-h
M-h` if there's a result block, or simply `M-h` if there isn't one, followed by
either `M-w` or `C-w`, depending on whether I want to save or kill, is perfectly
fast enough.

I also use `org-mode` for writing [my blog.](https://robbmann.io/posts)  With a little help from [an article](https://willschenk.com/articles/2019/using_org_mode_in_hugo/) we
have exporting to Hugo-specific markdown.  Without the export, Hugo can read Org
files _okay-ish_, but you wind up missing some nice QoL features, like header
links.

```emacs-lisp
(use-package ox-hugo)
```

`org-mode` provides `org-babel-tangle-jump-to-org`, which jumps back to an Org
source file from within the tangled code.  `renz/org-babel-tangle-jump-to-src`,
defined below, does the opposite - given the Org source file and point inside a
`src` block, it jumps to the location of the tangled code.  Provided by a helpful
[stackoverflow answer.](https://emacs.stackexchange.com/a/69591)

```emacs-lisp
(defun renz/org-babel-tangle-jump-to-src ()
  "The opposite of `org-babel-tangle-jump-to-org'.
Jumps at tangled code from org src block."
  (interactive)
  (if (org-in-src-block-p)
      (let* ((header (car (org-babel-tangle-single-block 1 'only-this-block)))
             (tangle (car header))
             (lang (caadr header))
             (buffer (nth 2 (cadr header)))
             (org-id (nth 3 (cadr header)))
             (source-name (nth 4 (cadr header)))
             (search-comment (org-fill-template
                              org-babel-tangle-comment-format-beg
                              `(("link" . ,org-id) ("source-name" . ,source-name))))
             (file (expand-file-name
                    (org-babel-effective-tangled-filename buffer lang tangle))))
        (if (not (file-exists-p file))
            (message "File does not exist. 'org-babel-tangle' first to create file.")
          (find-file file)
          (beginning-of-buffer)
          (search-forward search-comment)))
    (message "Cannot jump to tangled file because point is not at org src block.")))
```

Now we configure `org-mode` itself.  For a while I was trying `(setq
org-startup-indented t)` t get indentation under each header, but this was
interfering with the beautification features from `org-modern`.  Preferring the
latter over the former, I've removed the `org-startup-indented` call.

```emacs-lisp
(use-package org
  :hook
  ((org-mode . (lambda () (progn
                           (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                           (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors))))
   )

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b d" . org-babel-detangle)
   ("C-c o b o" . org-babel-tangle-jump-to-org)
   ("C-c o b s" . renz/org-babel-tangle-jump-to-src)
   ("C-c o j" . consult-org-heading)
   ("C-c o k" . org-babel-remove-result)
   ("C-c o o" . renz/jump-org)
   ("C-c o w" . renz/org-kill-src-block)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     (fortran . t)
     (julia . t)
     ;; (jupyter . t)
     (scheme . t)
     (haskell . t)
     (lisp . t)
     (clojure . t)
     (C . t)
     (org . t)
     (gnuplot . t)
     (awk . t)
     (latex . t)))
   ;; Outside the typical TODO/DONE states, I like to use DEAD as an indicator
   ;; that something is fully blocked, but not done.
   (setq org-todo-keywords '((sequence "TODO" "DEAD" "DONE")))
   (setq org-agenda-files '("~/.emacs.d/org/work.org")
         org-hugo-front-matter-format "yaml"))
```

`ob-async` adds asynchronous source block execution to some modes that otherwise wouldn't have it.

```emacs-lisp
(use-package ob-async

  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
                (require 'ob-sql-mode)
                (require 'hive2)))
  ;; Python has its own =:async yes= header argument we can use, so there's no
  ;; need to include it with ~ob-async~.
  (setq ob-async-no-async-languages-alist '("python"))
  ;; I'm having trouble rembering why I added this following line, except that I
  ;; belive it has something to do with exporting to HTML with syntax
  ;; highlighting.
  (setq org-html-htmlize-output-type 'css))
```


#### `org-modern` {#org-modern}

A [lovely look](https://github.com/minad/org-modern) for `org-mode` by minad.

```emacs-lisp
(use-package org-modern
  :after org
  :config
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")

  (if (display-graphic-p)
      (setq org-modern-table t)
    (setq org-modern-table nil))

  (global-org-modern-mode))
```


### SQL {#sql}

```emacs-lisp
(defun renz/sql-mode-hook ()
  (setq tab-width 4)
  (setq sqlformat-command 'sql-formatter))

(defvar renz/sql-indentation-offsets-alist
  '((syntax-error sqlind-report-sytax-error)
    (in-string sqlind-report-runaway-string)
    (comment-continuation sqlind-indent-comment-continuation)
    (comment-start sqlind-indent-comment-start)
    (toplevel 0)
    (in-block +)
    (in-begin-block +)
    (block-start 0)
    (block-end 0)
    (declare-statement +)
    (package ++)
    (package-body 0)
    (create-statement +)
    (defun-start +)
    (labeled-statement-start 0)
    (statement-continuation +)
    (nested-statement-open sqlind-use-anchor-indentation +)
    (nested-statement-continuation sqlind-use-previous-line-indentation)
    (nested-statement-close sqlind-use-anchor-indentation)
    (with-clause sqlind-use-anchor-indentation)
    (with-clause-cte +)
    (with-clause-cte-cont ++)
    (case-clause 0)
    (case-clause-item sqlind-use-anchor-indentation +)
    (case-clause-item-cont sqlind-right-justify-clause)
    (select-clause 0)
    (select-column sqlind-indent-select-column)
    (select-column-continuation sqlind-indent-select-column +)
    (select-join-condition ++)
    (select-table sqlind-indent-select-table)
    (select-table-continuation sqlind-indent-select-table +)
    (in-select-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (insert-clause 0)
    (in-insert-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (delete-clause 0)
    (in-delete-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)
    (update-clause 0)
    (in-update-clause sqlind-lineup-to-clause-end sqlind-right-justify-logical-operator)))

(defun renz/sql-indentation-offsets ()
  (setq sqlind-indentation-offsets-alist
        renz/sql-indentation-offsets-alist)
  (setq sqlind-basic-offset 4))

(add-hook 'sqlind-minor-mode-hook #'renz/sql-indentation-offsets)
(add-hook 'sql-mode-hook #'renz/sql-mode-hook)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlind-minor-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

(use-package hive2
  :after (sql)
  :mode ("\\.hql" . sql-mode))

(use-package ob-sql-mode
  :after (sql))
```

<!--list-separator-->

- <span class="org-todo todo TODO">TODO</span>  sql-formatter

    I've modified [sqlformat](https://github.com/purcell/sqlformat) for use with [sql-formatter](https://www.npmjs.com/package/sql-formatter), but I need to find a way to
    pass in a JSON of configuration values to the command line interface when we
    call it.

    ```emacs-lisp
    (use-package sqlformat
      :after (sql))
    ```


### Python {#python}

The `M-x compile` feature does not recognize or parse `pyright` error messages out
of the box, so I add that support myself.  Here's an example error message:

```text
/home/robb/tmp/errors.py/
  /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
  /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
  /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
    Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
2 errors, 1 warning, 0 informations
```

To get the basic `M-g M-n` and `M-g M-p` navigation working, we just need a regex to
parse file name, line, and column number.

```emacs-lisp
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
```

It would be nice if we could also capture the `\\(error\\|warning\\)` part as
"KIND", but I'm struggling to get it working.

Another nice vanilla feature of `python-mode` is `M-x python-check`, which runs a
pre-specified linter.  Setting that to `mypy` or `pyright` if either of those
programs exist is a small time saver.

```emacs-lisp
(use-package python
  :mode ("\\.py" . python-ts-mode)
  :config
  (if (executable-find "mypy")
      (setq python-check-command "mypy"))
  (if (executable-find "pyright")
      (setq python-check-command "pyright"))
  (add-hook 'python-mode-hook #'blacken-mode))
```

At one point, I ran into something similar to this [elpy issue](https://github.com/jorgenschaefer/elpy/issues/733) on Windows.  The
culprit was "App Execution Aliases" with python and python3 redirecting to the
windows store.  Using this fixed it:

```text
winkey -> Manage app execution aliases -> uncheck python and python3
```

Also on Windows - a `pip install` of `pyreadline3` is required to make
tab-completion work at all. It provides the `readline` import symbol.

Virtualenvs require `.dir-locals.el` to have something like:

```emacs-lisp
((python-mode . ((python-shell-virtualenv-root . "/path/to/my/.venv"))))
```

However, this only operates on \`run-python' shells.

`pyvenv` solves the otherwise very annoying problem of getting external tools like
\`compile' and \`eshell' to also use our virtual environment's python.  I may
still use `.dir-locals.el` to set things like the `python-check-command` on a
per-project basis, though.  I don't use `pyvenv` much now though, since the vast
majority of my development time is spent over Tramp, which `pyvenv` does not
support.  When I did, the code looked a bit like this:

```emacs-lisp
(when (package-installed-p 'pyvenv)
  (pyvenv-mode)
  ;; (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  ;; (pyvenv-tracking-mode)
  ;; (setenv "WORKON_HOME" "~/.conda/envs")
  )
```

For a while, it looks like Emacs was trying out something called [semantic-mode](https://www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html),
which looks a lot like a precursor to what we now know as the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/).  Enabling it was done through adding the `semantic-mode` hook to your
language's major mode hook:

```emacs-lisp
(add-hook 'python-mode-hook 'semantic-mode)
```

Don't mark the check command and virtualenv variables as unsafe.

```emacs-lisp
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
```

To have `eglot` always start up for a python buffer, we would tangle this line
into `init.el`.  However, this can cause a significant loading delay over Tramp,
and I would prefer snappy, simple access with LSP provided on an as-needed
basis.

```emacs-lisp
(add-hook 'python-mode-hook 'eglot-ensure)
```

Eventually, I would like to try the [emacs-jupyter](https://github.com/dzop/emacs-jupyter) package to interface with
Jupyter kernels from org-mode.


#### pyrightconfig.json, Tramp, and eglot {#pyrightconfig-dot-json-tramp-and-eglot}

The most consistent way to get `eglot` to properly configure the python virtual
environment with `pyright` is to have a static file at the root of the project,
called `pyrightconfig.json`.  I wrote a short plugin that allows me to select a
directory using `completing-read` and have Emacs write the content of
`pyrightconfig.json` based on what I selected, in the appropriate directory.

```emacs-lisp
(use-package pyrightconfig
  :after (python))
```

Configuring pyright this way rather than "activating" an environment through
Emacs (ala `pythonic-activate` or similar) means we can be running the language
server in more than one project at a time, each pointing to its respective
virtual environment.


#### blacken {#blacken}

Formatting a buffer with `black` has never been easier!

```emacs-lisp
(use-package blacken
  :bind ("C-c p" . blacken-mode)
  :after (python))
```


### Markdown {#markdown}

Some folks like to write markdown without hard line breaks.  When viewing those
documents, I can use the `renz/md-hook` to view it as if there were line breaks in
it.

```emacs-lisp
(defun renz/md-hook ()
  (visual-fill-column-mode)
  (setq-local fill-column 80))

(use-package markdown-mode
  ;; :config
  ;; (add-hook 'markdown-mode-hook #'renz/md-hook)
  )

(use-package poly-markdown
  :after (markdown-mode))
```


#### Code syntax in Markdown {#code-syntax-in-markdown}

Enable syntax highlighting within code fences for markdown

```emacs-lisp
(use-package poly-mode
  :mode ("\\.md" . poly-markdown-mode))
```


### AutoHotkey {#autohotkey}

```emacs-lisp
(use-package ahk-mode
  :mode "\\.ahk\\'")
```


### csv-mode {#csv-mode}

Handy for viewing data quickly.

```emacs-lisp
(use-package csv-mode
  :mode "\\.csv\\'")
```


## Small tool configuration {#small-tool-configuration}

Most of these are third party installs that require only a little configuration,
and don't warrant a big top-level header.


### Embark {#embark}

<https://github.com/oantolin/embark>

```emacs-lisp
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-\\" . embark-dwim)
   ("C-h B" . embark-bindings))

  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))


(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
```


### Coterm mode {#coterm-mode}

Adds the ability to use TUI programs in shell mode.

```emacs-lisp
(coterm-mode)
```


### Multiple cursors {#multiple-cursors}

A bit like multi-cursor for Jupyter or VSCode, but with a lot of configurable flexibility.

```emacs-lisp
(use-package multiple-cursors
  :config
  (global-unset-key (kbd "M-<down-mouse-1>"))
  (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))
```


### Visual fill column {#visual-fill-column}

For visual lines, this adds line breaks at the fill-column value.  Especially
useful for prose that is meant to be copied to other mediums, such as email or
word.

```emacs-lisp
(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
```


### Magit {#magit}

The one and only.

```emacs-lisp
(use-package magit)
```

As a reminder - when using pre-commit hooks it may take a while for the hooks to
install.  Magit will asynchronously kick off that process, and we can check on
it with `$`.  The built-in \`vc\` is _synchronous_, and will block Emacs entirely
until it's done.  So some of the performance hit from using Magit is well worth
it in situations like that.


### Change or copy inner/outer {#change-or-copy-inner-outer}

```emacs-lisp
(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . copy-inner)
         ("C-c y o" . copy-outer)))
```


### eww - search engine and browser {#eww-search-engine-and-browser}

Ecosia requires JavaScript, unfortunately.

```emacs-lisp
(use-package eww
  :config
  (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
```


### diff-hl {#diff-hl}

Adds highlighting to the fringe to see what's been added, deleted, or modified
from `git`'s perspective.

```emacs-lisp
(use-package diff-hl
  :bind ("C-c v" . diff-hl-mode))
```

Another option (which I haven't really looked at) is `git-gutter-fringe`.


### GNU Plot {#gnu-plot}

Scientific plotting - the old fashioned way!

```emacs-lisp
(use-package gnuplot
  :after (org))
```


### change-inner {#change-inner}

Modeled after Vim's `ci`, `ca`, `yi`, and `ya` commands, these let us yank or kill text
within a "surrounding" delimiter, such as "" or ().

```emacs-lisp
(use-package change-inner
  :bind (("C-c c i" . change-inner)
         ("C-c c o" . change-outer)
         ("C-c y i" . yank-inner)
         ("C-c y o" . yank-outer)))
```


### Esup: startup time profiling {#esup-startup-time-profiling}

[esup](https://github.com/jschaf/esup) is a tool for profiling the startup time of Emacs.  This snippet is a work
around of a bug where esup tries to step into the byte-compiled version of
\`cl-lib', and fails horribly: <https://github.com/jschaf/esup/issues/85>

```emacs-lisp
(use-package esup
  :bind ("C-c x p")
  :config
  (setq esup-depth 0))
```


### Reloading Emacs {#reloading-emacs}

Often used when changing up my `init.el`.

```emacs-lisp
(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))
```


### Language Server Protocol (LSP) with `eglot` {#language-server-protocol--lsp--with-eglot}

```emacs-lisp
(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))
```


#### Code block syntax highlighting for HTML export {#code-block-syntax-highlighting-for-html-export}

```emacs-lisp
(use-package htmlize
  :after (org))
```


#### Copying images out of org-babel {#copying-images-out-of-org-babel}

Offers two functions:

-   `ox-clip-formatted-copy`
-   `ox-clip-image-to-clipboard`

<!--listend-->

```emacs-lisp
(use-package ox-clip
  :after org)
```


## Start a server for `emacsclient` {#start-a-server-for-emacsclient}

```emacs-lisp
(server-start)
```


## Don't forget about these {#don-t-forget-about-these}

There are several other interesting options that I haven't tried out yet, including:

-   [ ] [org-download](https://github.com/abo-abo/org-download)
-   [ ] [math-delimiters](https://github.com/oantolin/math-delimiters)
-   [ ] [oantolin/placeholder](https://github.com/oantolin/placeholder)
-   [X] [emacs-eaf/emacs-application-framework](https://github.com/emacs-eaf/emacs-application-framework) &lt;--- big hassle
-   [X] [multiple-cursors](https://github.com/magnars/multiple-cursors.el)
-   [ ] [notmuch for email](https://notmuchmail.org/notmuch-emacs/)


## Footer {#footer}

Thank you for reading 'till the end or for being interested on how to end an
Emacs package.  So that's it, let's gracefully finish tangling everything:

```emacs-lisp
(provide 'init.el)
;;; init.el ends here
```
