---
title: "My Literate .emacs.d"
author: ["Robb Enzmann"]
draft: false
---

<div class="ox-hugo-toc toc">

<div class="heading">Table of Contents</div>

- [Goals](#goals)
- [Notable Features](#notable-features)
- [Tangling](#tangling)
- [Inspirations](#inspirations)
- [Header](#header)
- [Custom](#custom)
- [Packages](#packages)
- [OS-specific Configuration](#os-specific-configuration)
- [Theme: `ef-themes`](#theme-ef-themes)
- [Emacs' Built-in Settings](#emacs-built-in-settings)
- [Keybindings](#keybindings)
- [Text Completion](#text-completion)
- [Tramp](#tramp)
- [<span class="org-todo todo TODO">TODO</span> TreeSitter](#treesitter)
- [Language-specific major modes](#language-specific-major-modes)
- [Small tool configuration](#small-tool-configuration)
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
directory, meaning it's as simple as "clone-n-go".  Opening this document under
my configuration looks like so:

{{< figure src="https://user-images.githubusercontent.com/32076780/209576965-0c428bff-bea2-4b06-8373-37dfa4e4d86d.png" width="800px" >}}

If you prefer a prettier reading experience, check out this same document weaved
into [my website.](https://robbmann.io/emacsd/)  Or, if you're already reading this on my website, check out
the [source code on GitHub](https://github.com/renzmann/.emacs.d/).


## Goals {#goals}

If I had to sum up the theme of my configuration, it would be "vanilla extract".
In only a few instances do I change overt behavior of Emacs, the most noticeable
departure of course being the color theme with org-modern.  Even with those,
though, I want a configuration that fits my hands in such a way that I remain
comfortable using `emacs -Q` with very little disruption to my normal muscle
memory and workflow.

Aside from these aesthetic and philosophical reasons, there are practical
concerns this configuration needs to address.  I spend my time on Windows for
games, macOS or Linux with remote machines for work, and desktop Linux for
personal projects like building my website.  Some of these situations enforce a
very slow internet connection and tight security measures for Tramp, which can
cause modern, "live updating" features like `corfu` and `consult` to hang Emacs.  In
other cases, I have no access to the outside internet at all (so no ELPA or
MELPA updates).  Hence, keeping only a small number of external dependencies
under `elpa/` maximizes portability and maintainability between systems.

Altogether, I wind up using Emacs 28+ on all three of the major platforms, in both
GUI and TTY mode.  So this config is designed to work equally well for:

| platform | terminal | GUI | ssh + TTY | Tramp |
|----------|----------|-----|-----------|-------|
| Linux    | ✅       | ✅  | ✅        | ✅    |
| macOS    | ✅       | ✅  | ✅        | ✅    |
| Windows  | ❌       | ✅  | ❌        | ✅    |

Once I figure out how to get colors working in Windows terminal, I'll update
this table.  For now though, the 16 bit colors don't react to my color theme,
and so on Windows I rely singularly on GUI mode, rather than WSL or Emacs inside
Alacritty.


## Notable Features {#notable-features}

You may notice that despite the laudable goal of intended minimalism, this
document is is still quite long, as I have found many (ever increasing) quirky
behaviors of Emacs that I tweak.  Most of my time is spent in Org, SQL, Python,
Bash, and Markdown, so the majority of configuration lies around these sections.

I do make changes to things that I feel "should have been included."  Some
examples of this are:

1.  Additional major modes for languages like Markdown, Go, and Rust
2.  Error message support for `pyright` in a `*Compilation*` buffer
3.  Reasonable indentation behavior for SQL files
4.  Updating buffers automatically if their contents change on disk
5.  Syntax highlighting for Source blocks in Markdown
6.  Handling ANSI color escape codes in shell output, compilation, and VC buffers
7.  Ability to run TUI interfaces in comint-mode (shell, eshell) on Linux and
    macOS


## Tangling {#tangling}

My configuration is a single literate programming document, which is tangled
into the standard `init.el` and supporting files.  This is so I can keep track of
all the crazy things I try, and explain them inline with the final code I decide
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
-   [Luca's Literate Config](https://www.lucacambiaghi.com/vanilla-emacs/readme.html)


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
are built-in to Emacs 29+, which helps maximize stability and portability.

To avoid loading packages twice, [the manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html) recommends disabling
`package-enable-at-startup` in `init.el`.

```emacs-lisp
(setq package-enable-at-startup nil)
```

MELPA (Milypostman's Emacs Lisp Package Archive) is the largest repository for
elisp sources that aren't a part of the official GNU ELPA.  To install packages
from it, we need it on the `package-archives` list.

```emacs-lisp
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
```

I do not use the `:ensure t` keyword in `use-package` declarations to install
packages.  Instead, I rely on `M-x package-install` and `M-x package-delete`, and
only permit `use-package` to handle the configuration and loading of packages.  As
mentioned in the introduction, each package's source is explicitly included into
version control of my configuration, so I don't worry too much about pinning
package versions in this file.  When I want to update a package, I use `M-x
package-update`, the `package.el` user interface, or delete the package's source
folder and use `renz/package-sync` (defined below).  Should something go wrong, I
roll back to a previous commit.  So far, this method has been reliable for
keeping my `init.el` (this README), `custom.el`, the `package-selected-packages`
variable, and `elpa/` directory all in sync with one another.

```emacs-lisp
(defun renz/package-sync ()
  "Remove unused sources and install any missing ones"
  (interactive)
  (package-autoremove)
  (package-install-selected-packages))
```

There are also a few hand-made packages I keep around in a special
`.emacs.d/site-lisp` directory.

```emacs-lisp
(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
```


## OS-specific Configuration {#os-specific-configuration}


### Microsoft Windows {#microsoft-windows}


#### Compiling Emacs {#compiling-emacs}

Compiling Emacs on Windows can be a bit of a pain, but it's _mostly_ the same as
on \*nix.  First, I use [MSYS2-MinGW](https://www.msys2.org/) to compile everything.  From the MinGW
terminal (NOT the plain MSYS2 one), I install all of these dependencies

```text
pacman -Su \
        autoconf \
        autogen \
        automake \
        automake-wrapper \
        diffutils \
        git \
        guile \
        libgc \
        libguile \
        libltdl \
        libunistring \
        make \
        texinfo \
        mingw-w64-x86_64-binutils \
        mingw-w64-x86_64-bzip2 \
        mingw-w64-x86_64-cairo \
        mingw-w64-x86_64-crt-git \
        mingw-w64-x86_64-dbus \
        mingw-w64-x86_64-expat \
        mingw-w64-x86_64-fontconfig \
        mingw-w64-x86_64-freetype \
        mingw-w64-x86_64-gcc \
        mingw-w64-x86_64-gcc-libs \
        mingw-w64-x86_64-gdk-pixbuf2 \
        mingw-w64-x86_64-gettext \
        mingw-w64-x86_64-giflib \
        mingw-w64-x86_64-glib2 \
        mingw-w64-x86_64-gmp \
        mingw-w64-x86_64-gnutls \
        mingw-w64-x86_64-harfbuzz \
        mingw-w64-x86_64-headers-git \
        mingw-w64-x86_64-imagemagick \
        mingw-w64-x86_64-isl \
        mingw-w64-x86_64-libffi \
        mingw-w64-x86_64-libgccjit \
        mingw-w64-x86_64-libiconv \
        mingw-w64-x86_64-libjpeg-turbo \
        mingw-w64-x86_64-libpng \
        mingw-w64-x86_64-librsvg \
        mingw-w64-x86_64-libtiff \
        mingw-w64-x86_64-libwinpthread-git \
        mingw-w64-x86_64-libxml2 \
        mingw-w64-x86_64-mpc \
        mingw-w64-x86_64-mpfr \
        mingw-w64-x86_64-pango \
        mingw-w64-x86_64-pixman \
        mingw-w64-x86_64-winpthreads \
        mingw-w64-x86_64-xpm-nox \
        mingw-w64-x86_64-lcms2 \
        mingw-w64-x86_64-xz \
        mingw-w64-x86_64-zlib \
        tar \
        wget
```

Then, the configure scripts:

```text
./autogen.sh
./configure \
    --prefix=/c/emacs-29 \
    --with-native-compilation \
    --with-gnutls \
    --with-jpeg \
    --with-png \
    --with-rsvg \
    --with-tiff \
    --with-wide-int \
    --with-xft \
    --with-xml2 \
    --with-xpm \
    --without-dbus \
    --without-pop
```

And finally the command to compile:

```text
make --jobs=$(NPROC)
sudo make install
```


#### Configuration {#configuration}

Windows, funnily enough, has some trouble registering the Windows key as a
usable modifier for Emacs.  In fact, `s-l` will _never_ be an option, since it's
handled at the hardware level.  I also add a few nice-to-haves, like setting the
default shell to `pwsh` and explicitly pathing out `aspell`, which I always install
with `msys64`.

```emacs-lisp
(defun renz/windowsp ()
  "Are we on Microsoft Windows?"
  (memq system-type '(windows-nt cygwin ms-dos)))

(when (renz/windowsp)
  ;; Set a better font on Windows
  (set-face-attribute 'default nil :font "Hack NF-12")

  ;; Alternate ispell when we've got msys on Windows
  (setq ispell-program-name "aspell.exe")

  ;; Set default shell to pwsh
  (setq explicit-shell-file-name "pwsh"))
```

For a time I considered enabling the use of the winkey like this:

```emacs-lisp
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
```

Followed by enabling specific chords, such as "winkey+a":

```emacs-lisp
(w32-register-hot-key [s-a])
```

Since I've taken a more TTY-friendly approach for my config in general, where
super can be a bit tough to integrate with both the windowing application _and_
the terminal emulator, I've mostly given up on the GUI key in favor of other
chords, especially the `C-c` ones.


### macOS {#macos}

Launching Emacs from the typical application launcher or command-space usually
won't capture any modifications to `$PATH`, typically handled in a file like
`~/.profile` or `~/.bashrc`. So, the main configuration included here is from
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell).

```emacs-lisp
(when (eq system-type 'darwin)
  ;; Uncomment this if we can't install Hack Nerd font
  ;; (set-face-attribute 'default nil :font "Menlo-14")
  (set-face-attribute 'default nil :font "Hack Nerd Font Mono-13")
  (exec-path-from-shell-initialize))
```


### Linux {#linux}

Very little to do here.  Emacs on Linux seems to "just work".  When I have the
Hack font installed, I sometimes turn it on by manually evaluating this block,
though.

```emacs-lisp
(set-face-attribute 'default nil :font "Hack Nerd Font Mono-11")
```


## Theme: `ef-themes` {#theme-ef-themes}

[Prot's](https://protesilaos.com/) themes have been reliably legible in nearly every situation.  Now with
his new [ef-themes](https://protesilaos.com/emacs/ef-themes), they're pretty, too! The `ef-themes-headings` variable creates
larger, bolder headings when in [Org-mode](#org-mode), and `ef-themes-to-toggle` allows me to
quickly switch between preset light and dark themes depending on the ambient
light of the room I'm in.

```emacs-lisp
(use-package ef-themes
  :if (display-graphic-p)
  :demand t
  :bind ("C-c m" . ef-themes-toggle)

  :init
  (setq ef-themes-headings
        '((0 . (1.9))
          (1 . (1.3))
          (2 . (1.2))
          (3 . (1.1))
          (4 . (1.0))
          (5 . (1.0)) ; absence of weight means `bold'
          (6 . (1.0))
          (7 . (1.0))
          (t . (1.0))))
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

(defun renz/find-recent-file ()
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
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
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
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

Enable colors in the `*compilation*` buffer.  Provided by a
[helpful stackoverflow answer](https://stackoverflow.com/a/3072831/13215205).

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

The _menu_ bar, on the other hand `(menu-bar-mode)`, is very handy, and I only
disable it on Windows where it looks hideous.

```emacs-lisp
(when (renz/windowsp)
  (menu-bar-mode -1))
```

For newcomers to Emacs, I would strongly discourage disabling either of these,
since they lend a lot to discovering all its wonderful built-in features.


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

Eventually, some of the custom functions that I bound to convenient keys had a
logical abstraction, which I extract and put up here for them to use.

```emacs-lisp
(defun renz/--jump-section (dirname prompt extension)
  "For internal use: prompt for a file under `dirname' in the user
emacs config site with matching `extension' regexp"
  (find-file
   (concat dirname
           (completing-read prompt
                            (directory-files dirname nil extension)))))
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


### Overriding defaults {#overriding-defaults}

Some default bindings aren't useful for me, so I bind them to actions I take
more frequently.

```emacs-lisp
(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'
```


### C-c bindings {#c-c-bindings}

Emacs has [some standards](https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html) about where user-configured keys should go; `C-c
<letter>` is always free for users.  It may seem like overkill how I set a header
for each possible `C-c` combination, but it's incredibly handy when I want to jump
directly to one of these headings while in another buffer.  See
e.g. `renz/jump-init`, which allows me to narrow in on a particular key I'd like
to bind by leveraging `completing-read`.  If a `C-c <letter>` combination is missing
as a header, then I'm probably using it in a `:bind` statement with `use-package`
somewhere else.


#### `C-c b` build / compile {#c-c-b-build-compile}

```emacs-lisp
(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)
```


#### `C-c d` jump to a tag {#c-c-d-jump-to-a-tag}

```emacs-lisp
(defun renz/find-tag ()
  "Use completing-read to navigate to a tag"
  (interactive)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(global-set-key (kbd "C-c d") #'renz/find-tag)
```


#### `C-c f` hippie-expand {#c-c-f-hippie-expand}

```emacs-lisp
(global-set-key (kbd "C-c f") #'hippie-expand)
```


#### `C-c i` jump to a header in my configuration {#c-c-i-jump-to-a-header-in-my-configuration}

```emacs-lisp
(setq renz/site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(defun renz/jump-configuration ()
  "Prompt for a .el file in my site-lisp folder, then go there."
  (interactive)
  (renz/--jump-section renz/site-lisp-dir
                       "Elisp config files: "
                       ".*\.el$"))

(defun renz/jump-init ()
  (interactive)
  (find-file (expand-file-name "README.org" user-emacs-directory)))

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


#### `C-c r` find recent files {#c-c-r-find-recent-files}

```emacs-lisp
(global-set-key (kbd "C-c r") #'renz/find-recent-file)
```


#### `C-c s` shell {#c-c-s-shell}

```emacs-lisp
(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'term)
```


#### `C-c w` whitespace mode {#c-c-w-whitespace-mode}

```emacs-lisp
(global-set-key (kbd "C-c w") #'whitespace-mode)
```


#### `C-c` Other bindings {#c-c-other-bindings}

```emacs-lisp
(global-set-key (kbd "C-c ;") #'comment-line)  ; TTY-friendly
(global-set-key (kbd "C-c <DEL>") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c <SPC>") #'mark-sexp)  ;; TTY-friendly
```


### F5-F9 {#f5-f9}

Like the `C-c <letter>` bindings, these are reserved for users.  In practice, even
though there are few of these keys, I tend to forget which is which.  So I wind
up using things bound to my `C-c` keymaps instead.  The `C-c` kyes from a more
natural, nested language in my head, so it feels more like I'm "speaking Emacs"
that way.


## Text Completion {#text-completion}

Emacs offers incredible depth and freedom when configuring methods that
automatically complete text.  There are actually two things that
"autocompletion" can refer to in Emacs:

1.  [Minibuffer completion](https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html)
2.  [Completion at point](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html)

Emacs on its own does not have a nice pop-up-menu like Vim for completing text
at point.  For both the minibuffer and `completion-at-point` it uses a special
buffer called `*Completions*`, from which we can see (and optionally select) a
completion from potential candidates.  Before we get to tweak those settings,
though, we first need to oil the engine with an enhanced _completion style_


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
`list-packages`.  In either case, I can just type "`pack lis`" in the minibuffer to
find the correct one.

```emacs-lisp
(use-package orderless
  :config
  (add-to-list 'completion-styles 'orderless)
  (setq orderless-component-separator "[ &]")

  :custom
  (completion-category-overrides '((file (styles basic partial-completion)))))
```


### Nicer Display and Behavior of `*Completions*` {#nicer-display-and-behavior-of-completions}

With the _completion style_ set, we now have to configure the interface for
_displaying_ candidates as we type.  First, I want candidates displayed as a
single, vertical list.

```emacs-lisp
(setq completions-format 'one-column)
```

Also, when using the built-in completion-at-point, the `*Completions*` buffer can
sometimes take up the whole screen when there are a lot of candidates.

```emacs-lisp
(unless (version< emacs-version "29.0")
  (setq completions-max-height 15))
```

Some time ago, Prot wrote a package called [MCT](https://github.com/protesilaos/mct/blob/main/mct.el) (Minibuffer and Completions in
Tandem) that enhanced the default minibuffer and `*Completions*` buffer behavior
to act more like what we expect of a modern editor's auto-complete.  He
discontinued development of that project once it became clear that Emacs 29 was
going to include similar behavior as a configurable option.  These are the
options in question.

```emacs-lisp
(unless (version< emacs-version "29.0")
  (setq completion-auto-help 'visible
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort nil
        completions-header-format nil))
```

Another nice addition to Emacs 29 is the option to sort completion candidates
with any supplied function.  Below is one example provided by Prot, which
prioritzes history, followed by lexicographical order, then length.

```emacs-lisp
(defun renz/sort-by-alpha-length (elems)
  "Sort ELEMS first alphabetically, then by length."
  (sort elems (lambda (c1 c2)
                (or (string-version-lessp c1 c2)
                    (< (length c1) (length c2))))))

(defun renz/sort-by-history (elems)
  "Sort ELEMS by minibuffer history.
Use `mct-sort-sort-by-alpha-length' if no history is available."
  (if-let ((hist (and (not (eq minibuffer-history-variable t))
                      (symbol-value minibuffer-history-variable))))
      (minibuffer--sort-by-position hist elems)
    (renz/sort-by-alpha-length elems)))

(defun renz/completion-category ()
  "Return completion category."
  (when-let ((window (active-minibuffer-window)))
    (with-current-buffer (window-buffer window)
      (completion-metadata-get
       (completion-metadata (buffer-substring-no-properties
                             (minibuffer-prompt-end)
                             (max (minibuffer-prompt-end) (point)))
                            minibuffer-completion-table
                            minibuffer-completion-predicate)
       'category))))

(defun renz/sort-multi-category (elems)
  "Sort ELEMS per completion category."
  (pcase (renz/completion-category)
    ('nil elems) ; no sorting
    ('kill-ring elems)
    ('project-file (renz/sort-by-alpha-length elems))
    (_ (renz/sort-by-history elems))))

(unless (version< emacs-version "29.0")
  (setq completions-sort #'renz/sort-multi-category))
```

Ideally, I would have a function that prioritizes based on _relevance_, which is
not always a trivial algorithm.

What all of the above form isn't _quite_ the live-updating version that [Oantolnin](https://github.com/oantolin/live-completions),
MCT, or vertico offer, but it's pretty close.  The `*Completions*` buffer updates
after every `<SPC>`, which is the natural filtering mechanism for `orderless`.


### Completion at point {#completion-at-point}

By default, Emacs uses `M-TAB`, or the equivalent `C-M-i` for `completion-at-point`.
I'd much prefer to use the easier and more intuitive `TAB`.

```emacs-lisp
(setq tab-always-indent 'complete)
```

Again, we set `C-n` and `C-p` when completion-in-region is active for selecting
candidates.

```emacs-lisp
(unless (version< emacs-version "29.0")
  (define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
  (define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion))
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
  :defer t
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
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


## <span class="org-todo todo TODO">TODO</span> TreeSitter {#treesitter}

Emacs 29 added native [TreeSitter](https://tree-sitter.github.io/tree-sitter/) support.  TreeSitter is a new way of
incrementally parsing source code that offers superior navigation and syntax
highlighting.  To fully realize this benefit, however, it requires that we
install `tree-sitter` grammars independently from Emacs.  Right now, I'm using
[casouri's modules](https://github.com/casouri/tree-sitter-module), which I build and install under `~/.emacs.d/tree-sitter`, if
they don't already exist under `/usr/local/lib/` or `~/.local/lib`.

**REMINDER:** We could just use a little lisp here to download the module if on linux or mac...

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
(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
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


### TOML {#toml}

```emacs-lisp
(use-package conf-mode
  :mode ("\\.toml\\'" . conf-toml-mode))
```


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
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)))))

  :init
  (defun renz/jump-org ()
    "Prompt for an org file in my emacs directory, then go there."
    (interactive)
    (renz/--jump-section renz/org-home "Org files: " ".*\.org$"))

  :bind
  (("C-c o a" . org-agenda)
   ("C-c o b d" . org-babel-detangle)
   ("C-c o b o" . org-babel-tangle-jump-to-org)
   ("C-c o b s" . renz/org-babel-tangle-jump-to-src)
   ;; ("C-c o j" . consult-org-heading)
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
     ;; (fortran . t)
     ;; (julia . t)
     ;; (jupyter . t)
     ;; (scheme . t)
     ;; (haskell . t)
     (lisp . t)
     ;; (clojure . t)
     ;; (C . t)
     ;; (org . t)
     ;; (gnuplot . t)
     ;; (awk . t)
     ;; (latex . t)
     ))

  (setq org-agenda-files '("~/.emacs.d/org/work.org")
        org-hugo-front-matter-format "yaml"))
```

`ob-async` adds asynchronous source block execution to some modes that otherwise wouldn't have it.

```emacs-lisp
(use-package ob-async
  :after org
  :config
  (add-hook 'ob-async-pre-execute-src-block-hook
            #'(lambda ()
                (require 'ob-sql-mode)
                (require 'hive2)))
  ;; Python has its own =:async yes= header argument we can use, so there's no
  ;; need to include it with ~ob-async~.
  (setq ob-async-no-async-languages-alist '("python"))
  ;; I'm having trouble remembering why I added this following line, except that I
  ;; believe it has something to do with exporting to HTML with syntax
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


#### Exporting to Hugo {#exporting-to-hugo}

I also use `org-mode` for writing [my blog.](https://robbmann.io/posts)  With a little help from [an article](https://willschenk.com/articles/2019/using_org_mode_in_hugo/) we
have exporting to Hugo-specific markdown.  Without the export, Hugo can read Org
files _okay-ish_, but you wind up missing some nice QoL features, like header
links.

```emacs-lisp
(use-package ox-hugo
  :after org)
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

(use-package sql-indent
  :hook (sqlind-minor-mode . renz/sql-indentation-offsets))

(use-package sql-mode
  :hook ((sql-mode . renz/sql-mode-hook)
         (sql-mode . sqlup-mode)
         (sql-mode . sqlind-minor-mode)))

(use-package sqlup-mode
  :hook sql-interactive-mode)

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

    When I get to it, I think what I'll do instead is rewrite this to simply pipe
    the current buffer into `sql-formatter`, and use a bit of elisp to determine whether
    a `.sql-formatter-config.json` exists in the VC root directory.


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
  :config
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'blacken-mode)
  (add-hook 'python-mode-hook #'flymake-mode))
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

However, this only operates on \`run-python' shells.  Also, for projects, we need to
make sure that setting the virtualenv root is marked as safe.

```emacs-lisp
(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
```

Eventually, I would like to try the [emacs-jupyter](https://github.com/dzop/emacs-jupyter) package to interface with
Jupyter kernels from org-mode.


#### pyrightconfig.json {#pyrightconfig-dot-json}

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


#### Activating Virtual Environments Over Tramp {#activating-virtual-environments-over-tramp}

```emacs-lisp
(use-package tramp-venv
  :bind
  (("C-c t v a" . tramp-venv-activate)
   ("C-c t v d" . tramp-venv-deactivate)))
```


### Markdown {#markdown}

Some folks like to write markdown without hard line breaks.  When viewing those
documents, I can use `M-x renz/md-hook` to view it as if there were line breaks in
it.

```emacs-lisp
(defun renz/md-hook ()
  "View buffer in visual fill mode with 80 character width."
  (interactive)
  (visual-fill-column-mode)
  (setq-local fill-column 80))
```

I make a lot of spelling mistakes as I type...

```emacs-lisp
(add-hook 'text-mode-hook 'flyspell-mode)
```

`poly-markdown-mode` enables syntax highlighting within code fences for markdown.

```emacs-lisp
(use-package poly-markdown
  :after (markdown-mode)
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

These are tweaks for both third party packages and those bundled with emacs that
require little configuration and don't warrant a top-level header.


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


### dired {#dired}

By default, `dired` uses bytes instead of "K", "Mb", or "G" for file sizes.  I
also have it hide the mode, size, and owner of each file by default.

```emacs-lisp
(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))
```

Also enabled above is Do-What-I-Mean (DWIM) copying.  This is for when two dired
windows are open, and we want to copy something from one location to the other.
By enabling `dired-dwim-target`, it auto-populates the minibuffer with the other
dired window's path when issuing a copy command with `C`.


### Coterm mode {#coterm-mode}

Adds the ability to use TUI programs in shell mode.

```emacs-lisp
(use-package coterm
  :unless (renz/windowsp)
  :config
  (coterm-mode))
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
(use-package magit
  :bind ("C-c g" . magit-status))
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
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
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

As of version 29, [eglot](https://github.com/joaotavora/eglot) (Emacs polyGLOT) is bundled with Emacs.  It provides Emacs with the
client side configuration for the [language server protocol](https://microsoft.github.io/language-server-protocol/).

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

To have `eglot` always start up for a python buffer, we would tangle this line
into `init.el`.  However, this can cause a significant loading delay over Tramp,
and I would prefer snappy, simple access with LSP provided on an as-needed
basis.

```emacs-lisp
(add-hook 'python-mode-hook 'eglot-ensure)
```


#### Side show: `semantic-mode` {#side-show-semantic-mode}

For a while, it looks like Emacs was trying out something called [semantic-mode](https://www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html),
which looks a lot like a precursor to what we now know as the [Language Server
Protocol](https://microsoft.github.io/language-server-protocol/).  Enabling it was done through adding the `semantic-mode` hook to your
language's major mode hook:

```emacs-lisp
(add-hook 'python-mode-hook 'semantic-mode)
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
