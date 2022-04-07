---
title: "Data Scientist Discovers Terminal Isn't So Bad After All"
date: 2022-03-29T08:35:08-04:00
draft: false
---


# TODO: table of contents
# TODO: note on how we're addressing the most common challenges I see from my team members

It took me a while to admit it, but with a little upfront time investment, the
command line is probably the fastest way to get _most_ tasks accomplished.  For
me, _most_ tasks means short, one-off, common file operations, such as:

1. Trying to find that one damn file that's nested **somewhere** down all those
   folders
1. Figuring out what version(s) of python I have installed, and making new
   virtual environments
1. Downloading files from a link and unzipping the results
1. Examining and editing the contents of a file

Can an IDE or VScode offer all this to me? Yup. Those tools are awesome, but
when I'm on the phone with someone, and they say "hey, can you pull up that one
thing real fast?" I don't have time to boot JetBrains, nor do I want to dig
through VScode menus.  I pop open the terminal, fuzzy search where I need to go,
and hit the file with `vim` if it's text or `open`[^1] if it's something like Excel
or PowerPoint.  The process of getting that file open from a cold start is
around 10 seconds.  Let's say it takes on average around 1 minute to find a file
clicking through a file manager.  That's 50 seconds of savings per file.
Suppose we only look for around a dozen files like this per work day. Back
of napkin math tells us:

```
    50 seconds/file
  * 12 files/day
  *  5 days/wk
/ 3600 s/hr
-------------
     0.83 hours/wk
```

So assuming you work with roughly the numbers above, one hour of getting
comfortable with `fzf` will pay for itself in under two weeks.  Scale this
against the number of files you open and the dividends are much faster.  If your
work looks anything like mine, you're sifting through at least several dozen
spreadsheets, presentations, and source code files every day, many of them with
similar names but with `v3.pptx` or `v_FINAL.xlsx` put at the end.

# TODO side-by-side video of fuzzy find vs. window manager

Does everyone need to use a fuzzy-finder to find and open files?  Certainly not.
Some Unix die-hards [abhor the use of fuzzy-finders][romainl_comment] in their
workflow, but I just can't seem to get a pure "unixy" way to work nearly as fast
as `ctrl+t` followed by slapping the keyboard with letters that _might_ be
somewhere in that file name.[^2]  I also don't think comments in the spirit of
the linked `/u/romainl` comment have the same set of assumptions about what a
"typical" data science setup looks like.  I haven't worked professionally as a
website developer, but I have a feeling we work in _very_ different
environments.  Often I'm sitting in front of a data warehouse I've never
connected to before, with 2,000 unique table names, each with possibly 200+
columns.  Usually the first thing I do is write a small `fzf` window that lets
me search columns or table names.  "Are there any features related to customer
age? Did an excel sheet from last month make it into the data lake?"
Interactive, visual feedback as I type these things, followed by a `ctrl+u` to
clear the search bar is _way_ faster than building a pipe with `find` and/or
`grep` and examining the results each time.

# A note to Windows users

This article is largely aimed at a \*nix crowd, which means macOS and linux
users, for the most part.  The [windows terminal] is actually pretty slick, and
with the Windows Subsystem for Linux, you can follow along after installing the
latest Ubuntu from the windows store.  The other main option is cygwin/mingw2 to
get "unixy" tools on Windows, but for anyone just starting out I recommend the
WSL route.

# Python Environment Management

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

To usage of this function is like this:
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

# faster navigation with key remap and emacs bindings
	// maybe do this section last, once we know commands
	// hint at this section earlier

	// caps -> ctrl
	// alt+{f,b,d,h} ctrl+{f,b,d,h,a,e,k,y,r,u}
	// fzf plugin: ctrl+{t,r} alt+c; my install-fzf function

# treating dotfiles like cattle, not pets
	// git clone setup for all my dots
	// 

[^1]: This is a macOS command.  On Windows, just type the name of the file.  For
Linux I usually am on Gnome desktop, which uses `gio open`
[^2]: That said, `find . -name "whatever.csv"` is definitely still useful in a
  lot of cases

[romainl_comment]: <https://www.reddit.com/r/vim/comments/cp1upz/comment/ewnrslj/?utm_source=share&utm_medium=web2x&context=3>
[mental_game]: <https://youtu.be/UANN2Eu6ZnM?list=PLJHpE8rcAdmY-QUnKBOHgAy_0IEr-uMtF&t=341> "The Mental Game of Python - Raymond Hettinger"
