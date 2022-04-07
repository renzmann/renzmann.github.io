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

[Raymond Hettinger says][mental_game] we have a buffer in our mind of "about five things, plus
or minus two". By reducing the process above to only four steps, even on a bad
day I can remember how to do this.


[mental_game]: <https://youtu.be/UANN2Eu6ZnM?list=PLJHpE8rcAdmY-QUnKBOHgAy_0IEr-uMtF&t=341> "The Mental Game of Python - Raymond Hettinger"
