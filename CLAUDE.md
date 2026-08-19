# Build

```sh
python3 build.py   # build site to ./build
rm -rf ./build     # clean
```

Output goes to `build/`.
Deployed to GitHub Pages on push to main via `.github/workflows/publish.yml`.
Only use stdlib Python, no external dependencies or package manager.


# Posts

```html
layout: default
title: Getting Emacs 29 to Automatically Use Tree-sitter Modes
date: 2023-01-22
description: A tiny package that auto-falls-back between *-mode and *-ts-mode based on grammar availability.
---
<p>HTML body here.</p>
```
