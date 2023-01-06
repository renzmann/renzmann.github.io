---
title: "The *Completions* Buffer Gets a Big Upgrade in Emacs 29"
date: 2022-12-31T00:00:00-05:00
lastmod: 2023-01-06T10:51:39-05:00
categories: ["emacs"]
draft: false
weight: 2004
---

There's been a lot of talk about how `eglot` and `tree-sitter` will be distributed
with Emacs 29, but I've seen less buzz around the new functionality coming to
the vanilla &lowast;Completions&lowast; buffer.  Now, I've been an ardent [vertico](https://github.com/minad/vertico) +
[orderless](https://github.com/oantolin/orderless) + [marginalia](https://github.com/minad/marginalia/) + [corfu](https://github.com/minad/corfu) user since seriously picking up Emacs over the
summer, and when initially looking for options I found [Prot's MCT](https://protesilaos.com/emacs/mct) pretty
alluring.  I didn't choose it since he had already decided to [discontine
development](https://protesilaos.com/codelog/2022-04-14-emacs-discontinue-mct/) given upcoming changes in Emacs 29, and as of writing even he
opted for [vertico](https://git.sr.ht/~protesilaos/dotfiles/tree/437a303b90b3354ca1a1d08cb2f793183d1b4c48/item/emacs/.emacs.d/prot-emacs-modules/prot-emacs-completion.el#L141) and [corfu](https://git.sr.ht/~protesilaos/dotfiles/tree/437a303b90b3354ca1a1d08cb2f793183d1b4c48/item/emacs/.emacs.d/prot-emacs-modules/prot-emacs-completion.el#L300).

There is still that tempting, bitter fruit on the horizon though - maximizing
everything I can out of the vanilla Emacs experience.  Getting to that mythical
"vanilla extract" that keeps my muscle memory nearly entirely intact between
`emacs -Q` and my config (check out "Goals" in my [.emacs.d](https://robbmann.io/emacsd/#goals) to see the reasoning
behind why I would want this).

Now that `treesit.el`, `use-package`, and `eglot` are all merged into the `emacs-29`
branch, I finally decided to give our good old friend the &lowast;Completions&lowast; buffer
another try, so that you don't have to.

(Some verbiage below is taken directly from `C-h n` (`view-emacs-news`))


## New 'visible' and 'always' values for 'completion-auto-help' {#new-visible-and-always-values-for-completion-auto-help}

There are two new values to control the way the "&lowast;Completions&lowast;" buffer behaves
after pressing a 'TAB' if completion is not unique.

The (old) default value `t` always hides the completion buffer after some
completion is made.

```emacs-lisp
(setq completion-auto-help t)
```

{{< figure src="/ox-hugo/auto-help-t.gif" >}}

The value 'always' updates or shows the &lowast;Completions&lowast; buffer after any attempt
to complete, including the first time we press TAB.  Comparing to the one above,
notice that the buffer pops up as soon as I complete `~/.emacs.d/`.  Before, I had
to start another completion by typing `tra<TAB>`.  Also, after completing
`transient/`, the buffer once again updates with the contents of that directory.

```emacs-lisp
(setq completion-auto-help 'always)
```

{{< figure src="/ox-hugo/auto-help-always.gif" >}}

The value 'visible' is like 'always', but only updates the completions if they
are already visible.  The main difference in this one is that we don't get the
&lowast;Completions&lowast; buffer on the first TAB for `~/.emacs.d/`:

```emacs-lisp
(setq completion-auto-help 'visible)
```

{{< figure src="/ox-hugo/auto-help-visible.gif" >}}

If your goal is reduction of visual noise because you already know how a chain
of `TAB`'s are going to complete, then 'visible' seems like a good option.


## The &lowast;Completions&lowast; buffer can now be automatically selected. {#the-and-lowast-completions-and-lowast-buffer-can-now-be-automatically-selected-dot}

This was my biggest gripe with &lowast;Completions&lowast; and what made it downright unusable
for completion-at-point.  Here's what the current behavior looks like with
completion in a buffer:

```emacs-lisp
(setq completion-auto-select nil)
```

{{< figure src="/ox-hugo/auto-select-nil.gif" >}}

In the minibuffer, we've always had `M-v` to switch to &lowast;Completions&lowast;, but there
was no analogue for completion-in-region.  Now, in Emacs 29, we can set
`completion-auto-select` to one of `t` or `second-tab` to enable automatic selection
of the "&lowast;Completions&lowast;" buffer

```emacs-lisp
(setq completion-auto-select t)
```

{{< figure src="/ox-hugo/auto-select-t.gif" >}}

If the value is 'second-tab', then the first `TAB` will display "&lowast;Completions&lowast;",
and the second one will switch to the "&lowast;Completions&lowast;" buffer.

```emacs-lisp
(setq completion-auto-select 'second-tab)
```

{{< figure src="/ox-hugo/auto-select-second-tab.gif" >}}

With 'second-tab', I can use the "&lowast;Completions&lowast;" buffer a lot like how I would
use `corfu`: type a bit, request completion with TAB, examine the list, and keep
typing to narrow the candidates, and request completion again.  If I see the
option I like, I just hit TAB a few times to get it.


## New commands for navigating completions from the minibuffer. {#new-commands-for-navigating-completions-from-the-minibuffer-dot}

-   `M-<up>` and `M-<down>` for `minibuffer-next-completion` and `minibuffer-previous-completion`
-   `M-RET` to choose active candidate
-   `C-u M-RET` to insert active candidate without exiting minibuffer
-   `C-x <up>` (`minibuffer-complete-history`) is like `minibuffer-complete` but
    completes on the history items instead of the default completion table.
-   `C-x <down>` (`minibuffer-complete-defaults`) is like `minibuffer-complete`, but
    completes on the default items instead of the completion table.

The first two also work for `completion-at-point` (in-buffer completion).

{{< figure src="/ox-hugo/completion-nav-commands.gif" >}}

Some may find the arrow keys an unfortunate choice, though, and bind completion
something more convenient:

```emacs-lisp
;; Up/down when completing in the minibuffer
(define-key minibuffer-local-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key minibuffer-local-map (kbd "C-n") #'minibuffer-next-completion)

;; Up/down when competing in a normal buffer
(define-key completion-in-region-mode-map (kbd "C-p") #'minibuffer-previous-completion)
(define-key completion-in-region-mode-map (kbd "C-n") #'minibuffer-next-completion)
```

My apologies to [Mohamed Suliman](https://www.scss.tcd.ie/~sulimanm/posts/default-emacs-completion.html), since I was also not able to figure out a fix
for `eshell` that permits the use of `M-<up>` and `M-<down>` with `M-RET`.  The issue
there, it seems, is that `eshell` uses its own `pcomplete` instead of
`completion-at-point`, which comes from `minibuffer.el`.  I have, however, had
success simply using `TAB` and `BACKTAB` with `RET`, by setting `completion-auto-select`
to `'second-tab`, as shown above.


## New user option 'completions-sort'. {#new-user-option-completions-sort-dot}

Much like how oantolin's [live-completions](https://github.com/oantolin/live-completions) gave us a way to sort candidates in
&lowast;Completions&lowast;, we now have a built-in method for specifying the sorting
function.  I took inspiration from [Prot's MCT documentation](https://github.com/protesilaos/mct#101-sort-completion-candidates-on-emacs-29) here to put
candidates I use frequently near the top, followed by the length of their name.

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

(setq completions-sort #'renz/sort-multi-category)
```


## Other Niceties {#other-niceties}

-   `completions-max-height` limits the height of the "&lowast;Completions&lowast;" buffer
-   `completions-header-format` is a string to control the heading line to show in
    the "&lowast;Completions&lowast;" buffer before the list of completions


## Do We Stick With Vanilla Extract? {#do-we-stick-with-vanilla-extract}

Now the fun part - let's tally pros and cons to see if I should abandon
everything for the Vanilla behavior:

| property                                           | score |
|----------------------------------------------------|-------|
| Consistent minibuffer + CAP                        | +1    |
| Vanilla GUI + TTY support                          | +1    |
| No marginalia for sole completion                  | -0.5  |
| Extra key press to cycle/complete                  | -0.5  |
| Candidates not buffered until requested            | -2    |
| Eyes shift focus to another part of screen for CAP | -0.5  |
| Total                                              | -1.5  |

In my typical day, I need to have a working TTY _and_ GUI version of Emacs, so
when something _just works_ for both, that's a +1 for me.  Corfu does have
[corfu-terminal](https://codeberg.org/akib/emacs-corfu-terminal), but it's maintained separately.  Also, having a consistent
interface for both the minibuffer and completion-at-point shrinks the
configuration domain, making it easier to maintain my config over time.

Unfortunately, in the case that there's only one completion candidate,
marginalia isn't triggered, so I don't get to see a key binding or flavor text
alongside the candidate I choose.  Vanilla Emacs will remind me about what key
combination I _could_ have used, which I can check any time with `C-h e` (the
&lowast;Messages&lowast; buffer), and I can use `C-h f` directly from the minibuffer, so this
only get -0.5.  The fact that I need extra key strikes compared to something
like Corfu's Tab-N-Go is an annoyance, but just requires a bit of muscle memory
change.  The real impasse here, though, is that candidates aren't shown until
requested.  I think Prot summed it up best here:

> Vertico has official extensions which can make it work exactly like MCT without
> any of MCT’s drawbacks. These extensions can also expand Vertico’s powers such
> as by providing granular control over the exact style of presentation for any
> given completion category (e.g. display Imenu in a separate buffer, show the
> switch-to-buffer list horizontally in the minibuffer, and present find-file in a
> vertical list—whatever the user wants).

So will I stick with just &lowast;Completions&lowast;?  No, probably not.  But these changes
do put the default completion system squarely in the "usable" category, which
I'm not sure I could have said before Emacs 29.  I will give it an
honest chance to see just how far I can push it, [much in the spirit of MCT](https://github.com/protesilaos/mct#12-alternatives),
before switching Vertico and Corfu back on.
