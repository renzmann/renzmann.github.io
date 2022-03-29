---
title: "Data Scientist Discovers Terminal Isn't So Bad After All"
date: 2022-03-29T08:35:08-04:00
draft: false
---

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
Suppose we only look for three files like this per work day. Now back
of napkin math tells us:

    50s * 3 files/day * 5 day/wk * 50 work weeks / 60s/hr / 24hr/day = 26 days

A _month_ of time accumulated over a working year, just _opening files_.  Does
everyone need to use a fuzzy-finder to find and open files?  Certainly not, but
if you're even a semi-technical user who needs to navigate through lots of
folders to get things to your colleagues, this investment pays for itself pretty
quickly

# TODO side-by-side video of fuzzy find vs. window manager

[1]: This is a macOS command, just type the name of the file on Windows and on
Linux I usually am on Gnome desktop, which uses `gio open`
