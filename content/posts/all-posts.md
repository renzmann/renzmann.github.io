---
author: ["Robb"]
lastmod: 2022-11-19T17:29:18-05:00
draft: false
---

## Emacs <span class="tag"><span class="_emacs">@emacs</span></span> {#emacs}

All posts in here will have the category set to _emacs_


### <span class="org-todo todo TODO">TODO</span> Fully Remote Python Development in Emacs <span class="tag"><span class="python">python</span><span class="remote">remote</span><span class="lsp">lsp</span></span> {#emacs-remote-python}


#### Lay of the land {#lay-of-the-land}

<!--list-separator-->

-  TRAMP vs. SSH + TTY


#### Remote virtual environments {#remote-virtual-environments}


#### Remote LSP {#remote-lsp}


### <span class="org-todo todo TODO">TODO</span> Moving My Emacs Configuration to a Literate Programming DocumentLiterate {#moving-my-emacs-configuration-to-a-literate-programming-documentliterate}


### <span class="org-todo todo TODO">TODO</span> Getting `eglot` + `pyright-langserver` to Use the Correct Virtual Environment (With Tramp Support!) <span class="tag"><span class="python">python</span><span class="lsp">lsp</span><span class="eglot">eglot</span><span class="tramp">tramp</span><span class="remote">remote</span></span> {#emacs-eglot-pyrightconfig}

The most reliable setup for developing Python projects on remote hosts with LSP support
for me so far has been with [eglot](https://github.com/joaotavora/eglot) and [pyright](https://github.com/microsoft/pyright).  I've also tried [lsp-mode](https://emacs-lsp.github.io/lsp-mode/) with `pyright`, and
both of `lsp-mode` and `eglot` with the [python-lsp-server](https://github.com/python-lsp/python-lsp-server), however I've landed on `eglot` +
`pyright` for a few reasons:

1.  Fewest number of Tramp hangs.  This _could_ just be a symptom of my particuar setup,
    though.
2.  `eglot` will have built-in support in future Emacs versions.  This may or may not be
    worth a damn to other Emacs users.
3.  `pyright` has been strictly faster at error checking and diagnostic updates as
    compared to `python-language-server` in the machines I'm using.

One hiccup remained though: `pyright` is typically a system or user installation, not
something you install per virtual environment.  Getting `pyright` to see the correct virtual
environment, and correctly report which dependencies are installed was a bit of a hassle,
but I think my favorite solution so far has been to configure the virtual environment
through the `pyrightconfig.json` file at the root of my projects, and just have this file
ignored by git.  Typically, this file looks like this:

```js
{
    "venvPath": "/absolute/path/to/dir/",
    "venv": ".venv"
}
```

I'm pretty happy with the other default configurations for `pyright`, so I leave those be,
and just configure the virtual envrionment path this way.  What was annoying me, though,
is that I'd need to write out this absolute path for each machine I clone a project into,
since relative paths and shortcuts using `~` aren't supported.  Much better if we can just
have Emacs do it for us.

In the spirit of other Emacs/Python tools like `pythonic` and `pyvenv` for activating virtual
environments, I wanted something that would just prompt for a directory using
`completing-read`, and then populate the contents of `pyrightconfig.json` automatically based
on my selection.

To start, let's assume I've got the `venvPath` and `venv` as strings already, and I just want to format it as JSON text that we'll dump into a file later.

```emacs-lisp
(defun pyrightconfig--json-contents (venvPath venv)
  (format "{
    \"venvPath\": \"%s\",
    \"venv\": \"%s\"
}" venvPath venv))
```

Now the more interesting part.  We really just need to do three things:

1.  Prompt for a directory that houses a Python virtual environment
2.  Break the result into an absolute parent path + base name, cleaning any Tramp prefix in
    the process
3.  Write the contents of `pyrightconfig--json-contents` using the previous result to a file
    in the version control root.

It's worth mentioning that we _must_ put this file in the VC root, otherwise `eglot` just
won't pick it up.  For my purposes, the VC system will always be git, so I'm going to make
an assumption here and use `vc-git-root` instead of something more generic.

```emacs-lisp
(defun pyrightconfig-write (virtualenv)
  (interactive "DEnv: ")
  ;; Naming convention for venvPath matches the field for pyrightconfig.json
  (let* ((venv-dir (tramp-file-local-name (file-truename virtualenv)))
         (venv-file-name (directory-file-name venv-dir))
         (venvPath (file-name-directory venv-file-name))
         (venv (file-name-base venv-file-name))
         (base-dir (vc-git-root default-directory))
         (out-file (expand-file-name "pyrightconfig.json" base-dir))
         (out-contents (pyrightconfig--json-contents venvPath venv)))
    (with-temp-file out-file (insert out-contents))))
```

Before getting into the nitty-gritty of how this works, here's a quick demo:

{{< figure src="/ox-hugo/pyrightconfig-demo.gif" >}}
