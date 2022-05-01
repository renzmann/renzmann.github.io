---
title: "Manage Python Environments Faster With Aliases and Functions"
date: 2022-04-06T20:03:03-04:00
draft: false
---

If you aren't regularly wiping and rebuilding your virtual environments, you
should be; anyone trying to run your project for the first time will thank you
for it. For most folks that's using `python -m venv .venv` to create a new one,
or `conda create` if you're on the conda stack.  I do this so frequently that it
became my first _bash alias_.  In `sh`, `bash`, and most other shells, there's
more than one way to make longer commands shorter, the easiest of which is the
alias:

```sh
alias so="source .venv/bin/activate"
```

After running this, when we use `so` as the _first_ command, it will be
replaced with the text `source .venv/bin/activate`.  Remembering to just type
`so` once I `cd` to a project is much easier to remember and type quickly.
If we don't already have an environment, we usually have to create it:

```sh
alias new-venv="python -m venv .venv"
```

To chain two operations together in `bash`, we use `&&` to allow the second part
to run only if the first one succeeded:

```sh
new-venv && so
```

Beyond that, we usually have to upgrade `pip` in the new environment

```sh
new-venv && so && pip install --upgrade pip wheel
```

And this chain is so common, I actually have the entire thing under the
`new-venv` name, but as a bash `function` so it can take arguments:

```bash
new-venv() {
	local name=${1:-.venv}
	local python_version=${2:-3}

	python${python_version} -m venv $name \
		&& source $name/bin/activate \
		&& python3 -m pip install --upgrade pip wheel
}
```

The usage of this function is like this:
```
new-venv [NAME] [PYTHON_VERSION]
```
with both arguments optional.  The voodoo magic on the first two lines inside
the function just says:

1. Assign the value of the first argument to the `name` variable, and set it to
   ".venv" if nothing is passed in
1. Assign the value of the second parameter to the `python_version` variable,
   and set it to "3" if nothing is passed in

Physical savings might only be a few letters, but there's a real cognitive
benefit to building out your most common operations as aliases or functions.
You can think at higher levels of operation, with four to five commands clicked
together instead of just the current one.  Often I go "I call uncle! Let's try
with a fresh environment"

```sh
deactivate && rm -rf .venv && new-venv && poetry install
```

[Raymond Hettinger says][mental_game] we have a buffer in our mind of "about
five things, plus or minus two". By reducing the process above to only four
steps, even on a bad day I can remember how to do this.

To make sure these aliases/functions are available every time you log in, add
them to your "rc" file.  For most folks that's `~/.bashrc`, but fish users would
use `funced` and `funcsave`, `zsh` users have `~/.zshrc`, and on Windows
there's [a host of options][powershell_profile] (I wouldn't try all this in
`cmd`).

I actually like the `funced` idea from fish a lot, so I use something
[similar][funced].  This allows me to edit any function in my
`~/.bash_functions` folder, which is then loaded up using `load-funcs` (also a
function), and _that_ is what gets executed by [my `~/.bashrc`][bashrc].  This
gives me the chance to very quickly save useful snippets like what's above for
later.  In particular, instead of looking up the right invocation to [install
poetry][poetry] every time, I just tucked it away into the
[`install-poetry`][install_poetry] function when I first ran it, and now it's
ready for me everywhere I take my dotfiles.


[mental_game]: <https://youtu.be/UANN2Eu6ZnM?list=PLJHpE8rcAdmY-QUnKBOHgAy_0IEr-uMtF&t=341> "The Mental Game of Python - Raymond Hettinger"
[powershell_profile]: <https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_profiles?view=powershell-7.2>
[funced]: <https://github.com/renzmann/renzmann/blob/13e5d003ab9ea395aae0a1106ded585786ed3c20/.bash_functions/funced.sh#L2>
[bashrc]: <https://github.com/renzmann/renzmann/blob/13e5d003ab9ea395aae0a1106ded585786ed3c20/.bashrc#L16>
[poetry]: <https://python-poetry.org/docs/master/#installing-with-the-official-installer>
[install_poetry]: <https://github.com/renzmann/renzmann/blob/394ab71991c41b6c5bb207cd3389ad803045dc06/.bash_functions/install-poetry.sh#L2>
