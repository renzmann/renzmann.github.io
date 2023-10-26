---
title: "My Literate .emacs.d"
author: ["Robb Enzmann"]
draft: false
---
<?xml version="1.0" encoding="utf-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en">
<head>
<!-- 2023-10-26 Thu 16:48 -->
<meta http-equiv="Content-Type" content="text/html;charset=utf-8" />
<meta name="viewport" content="width=device-width, initial-scale=1" />
<title>My Literate =.emacs.d=</title>
<meta name="author" content="Robb Enzmann" />
<meta name="generator" content="Org Mode" />
<style>
  #content { max-width: 60em; margin: auto; }
  .title  { text-align: center;
             margin-bottom: .2em; }
  .subtitle { text-align: center;
              font-size: medium;
              font-weight: bold;
              margin-top:0; }
  .todo   { font-family: monospace; color: red; }
  .done   { font-family: monospace; color: green; }
  .priority { font-family: monospace; color: orange; }
  .tag    { background-color: #eee; font-family: monospace;
            padding: 2px; font-size: 80%; font-weight: normal; }
  .timestamp { color: #bebebe; }
  .timestamp-kwd { color: #5f9ea0; }
  .org-right  { margin-left: auto; margin-right: 0px;  text-align: right; }
  .org-left   { margin-left: 0px;  margin-right: auto; text-align: left; }
  .org-center { margin-left: auto; margin-right: auto; text-align: center; }
  .underline { text-decoration: underline; }
  #postamble p, #preamble p { font-size: 90%; margin: .2em; }
  p.verse { margin-left: 3%; }
  pre {
    border: 1px solid #e6e6e6;
    border-radius: 3px;
    background-color: #f2f2f2;
    padding: 8pt;
    font-family: monospace;
    overflow: auto;
    margin: 1.2em;
  }
  pre.src {
    position: relative;
    overflow: auto;
  }
  pre.src:before {
    display: none;
    position: absolute;
    top: -8px;
    right: 12px;
    padding: 3px;
    color: #555;
    background-color: #f2f2f299;
  }
  pre.src:hover:before { display: inline; margin-top: 14px;}
  /* Languages per Org manual */
  pre.src-asymptote:before { content: 'Asymptote'; }
  pre.src-awk:before { content: 'Awk'; }
  pre.src-authinfo::before { content: 'Authinfo'; }
  pre.src-C:before { content: 'C'; }
  /* pre.src-C++ doesn't work in CSS */
  pre.src-clojure:before { content: 'Clojure'; }
  pre.src-css:before { content: 'CSS'; }
  pre.src-D:before { content: 'D'; }
  pre.src-ditaa:before { content: 'ditaa'; }
  pre.src-dot:before { content: 'Graphviz'; }
  pre.src-calc:before { content: 'Emacs Calc'; }
  pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
  pre.src-fortran:before { content: 'Fortran'; }
  pre.src-gnuplot:before { content: 'gnuplot'; }
  pre.src-haskell:before { content: 'Haskell'; }
  pre.src-hledger:before { content: 'hledger'; }
  pre.src-java:before { content: 'Java'; }
  pre.src-js:before { content: 'Javascript'; }
  pre.src-latex:before { content: 'LaTeX'; }
  pre.src-ledger:before { content: 'Ledger'; }
  pre.src-lisp:before { content: 'Lisp'; }
  pre.src-lilypond:before { content: 'Lilypond'; }
  pre.src-lua:before { content: 'Lua'; }
  pre.src-matlab:before { content: 'MATLAB'; }
  pre.src-mscgen:before { content: 'Mscgen'; }
  pre.src-ocaml:before { content: 'Objective Caml'; }
  pre.src-octave:before { content: 'Octave'; }
  pre.src-org:before { content: 'Org mode'; }
  pre.src-oz:before { content: 'OZ'; }
  pre.src-plantuml:before { content: 'Plantuml'; }
  pre.src-processing:before { content: 'Processing.js'; }
  pre.src-python:before { content: 'Python'; }
  pre.src-R:before { content: 'R'; }
  pre.src-ruby:before { content: 'Ruby'; }
  pre.src-sass:before { content: 'Sass'; }
  pre.src-scheme:before { content: 'Scheme'; }
  pre.src-screen:before { content: 'Gnu Screen'; }
  pre.src-sed:before { content: 'Sed'; }
  pre.src-sh:before { content: 'shell'; }
  pre.src-sql:before { content: 'SQL'; }
  pre.src-sqlite:before { content: 'SQLite'; }
  /* additional languages in org.el's org-babel-load-languages alist */
  pre.src-forth:before { content: 'Forth'; }
  pre.src-io:before { content: 'IO'; }
  pre.src-J:before { content: 'J'; }
  pre.src-makefile:before { content: 'Makefile'; }
  pre.src-maxima:before { content: 'Maxima'; }
  pre.src-perl:before { content: 'Perl'; }
  pre.src-picolisp:before { content: 'Pico Lisp'; }
  pre.src-scala:before { content: 'Scala'; }
  pre.src-shell:before { content: 'Shell Script'; }
  pre.src-ebnf2ps:before { content: 'ebfn2ps'; }
  /* additional language identifiers per "defun org-babel-execute"
       in ob-*.el */
  pre.src-cpp:before  { content: 'C++'; }
  pre.src-abc:before  { content: 'ABC'; }
  pre.src-coq:before  { content: 'Coq'; }
  pre.src-groovy:before  { content: 'Groovy'; }
  /* additional language identifiers from org-babel-shell-names in
     ob-shell.el: ob-shell is the only babel language using a lambda to put
     the execution function name together. */
  pre.src-bash:before  { content: 'bash'; }
  pre.src-csh:before  { content: 'csh'; }
  pre.src-ash:before  { content: 'ash'; }
  pre.src-dash:before  { content: 'dash'; }
  pre.src-ksh:before  { content: 'ksh'; }
  pre.src-mksh:before  { content: 'mksh'; }
  pre.src-posh:before  { content: 'posh'; }
  /* Additional Emacs modes also supported by the LaTeX listings package */
  pre.src-ada:before { content: 'Ada'; }
  pre.src-asm:before { content: 'Assembler'; }
  pre.src-caml:before { content: 'Caml'; }
  pre.src-delphi:before { content: 'Delphi'; }
  pre.src-html:before { content: 'HTML'; }
  pre.src-idl:before { content: 'IDL'; }
  pre.src-mercury:before { content: 'Mercury'; }
  pre.src-metapost:before { content: 'MetaPost'; }
  pre.src-modula-2:before { content: 'Modula-2'; }
  pre.src-pascal:before { content: 'Pascal'; }
  pre.src-ps:before { content: 'PostScript'; }
  pre.src-prolog:before { content: 'Prolog'; }
  pre.src-simula:before { content: 'Simula'; }
  pre.src-tcl:before { content: 'tcl'; }
  pre.src-tex:before { content: 'TeX'; }
  pre.src-plain-tex:before { content: 'Plain TeX'; }
  pre.src-verilog:before { content: 'Verilog'; }
  pre.src-vhdl:before { content: 'VHDL'; }
  pre.src-xml:before { content: 'XML'; }
  pre.src-nxml:before { content: 'XML'; }
  /* add a generic configuration mode; LaTeX export needs an additional
     (add-to-list 'org-latex-listings-langs '(conf " ")) in .emacs */
  pre.src-conf:before { content: 'Configuration File'; }

  table { border-collapse:collapse; }
  caption.t-above { caption-side: top; }
  caption.t-bottom { caption-side: bottom; }
  td, th { vertical-align:top;  }
  th.org-right  { text-align: center;  }
  th.org-left   { text-align: center;   }
  th.org-center { text-align: center; }
  td.org-right  { text-align: right;  }
  td.org-left   { text-align: left;   }
  td.org-center { text-align: center; }
  dt { font-weight: bold; }
  .footpara { display: inline; }
  .footdef  { margin-bottom: 1em; }
  .figure { padding: 1em; }
  .figure p { text-align: center; }
  .equation-container {
    display: table;
    text-align: center;
    width: 100%;
  }
  .equation {
    vertical-align: middle;
  }
  .equation-label {
    display: table-cell;
    text-align: right;
    vertical-align: middle;
  }
  .inlinetask {
    padding: 10px;
    border: 2px solid gray;
    margin: 10px;
    background: #ffffcc;
  }
  #org-div-home-and-up
   { text-align: right; font-size: 70%; white-space: nowrap; }
  textarea { overflow-x: auto; }
  .linenr { font-size: smaller }
  .code-highlighted { background-color: #ffff00; }
  .org-info-js_info-navigation { border-style: none; }
  #org-info-js_console-label
    { font-size: 10px; font-weight: bold; white-space: nowrap; }
  .org-info-js_search-highlight
    { background-color: #ffff00; color: #000000; font-weight: bold; }
  .org-svg { }
</style>
</head>
<body>
<div id="content" class="content">
<div id="table-of-contents" role="doc-toc">
<h2>Table of Contents</h2>
<div id="text-table-of-contents" role="doc-toc">
<ul>
<li><a href="#orge60ec89">Goals</a></li>
<li><a href="#org75d1ce9">Notable Features</a></li>
<li><a href="#orgb197653">Tangling</a></li>
<li><a href="#org0ce0eb4">Inspirations</a></li>
<li><a href="#org627339f">Getting Emacs</a></li>
<li><a href="#orgbf3faba">Header</a></li>
<li><a href="#org5acf5d4">Custom</a></li>
<li><a href="#orgaed21b5">Proxy settings</a></li>
<li><a href="#org31e6402">Packages</a></li>
<li><a href="#org76d40d4">OS-specific Configuration</a></li>
<li><a href="#org4934940">Font</a></li>
<li><a href="#org4781c04">Theme</a></li>
<li><a href="#orga321a2d">Emacs' Built-in Settings</a></li>
<li><a href="#org173911a">Keybindings</a></li>
<li><a href="#orgdf681ce">Text Completion</a></li>
<li><a href="#orgba42f05">Language-specific major modes</a></li>
<li><a href="#orgf8e4410">Tool configuration</a></li>
<li><a href="#org95eef66">Footer</a></li>
</ul>
</div>
</div>
<p>
Want to use it? Go ahead!
</p>

<div class="org-src-container">
<pre class="src src-shell">git clone https://github.com/renzmann/.emacs.d ~/.emacs.d
</pre>
</div>

<p>
All external dependency sources are explicitly included under the <code>elpa/</code>
directory, meaning it's as simple as "clone-n-go".  Opening this document under
my configuration looks like so:
</p>


<div id="org943b60c" class="figure">
<p><img src="https://raw.githubusercontent.com/renzmann/.emacs.d/e93d5ea85896745ed5e79fa725278cd37f09f727/img/emacs-screen.png" alt="emacs-screen.png" width="800px" />
</p>
</div>

<p>
If you prefer a prettier reading experience, check out this same document weaved
into <a href="https://robbmann.io/emacsd/">my website.</a>  Or, if you're already reading this on my website, check out
the <a href="https://github.com/renzmann/.emacs.d/">source code on GitHub</a>.
</p>

<div id="outline-container-orge60ec89" class="outline-2">
<h2 id="orge60ec89">Goals</h2>
<div class="outline-text-2" id="text-orge60ec89">
<p>
If I had to sum up the theme of my configuration, it would be "vanilla extract,"
because in only a few instances do I change the overt behavior of Emacs.  Even
with those, though, I want a configuration that fits my hands in such a way that
I remain comfortable using <code>emacs -Q</code> with very little disruption to my normal
muscle memory and workflow.
</p>

<p>
Aside from these aesthetic and philosophical reasons, there are practical
concerns this configuration needs to address.  I spend my time on Windows for
games, macOS or Linux with remote machines for work, and desktop Linux for
personal projects like building my website.  Some of these situations enforce a
very slow internet connection and tight security measures for Tramp, which can
cause modern, "live updating" features like <code>corfu</code> and <code>consult</code> to hang Emacs.
In other cases, I have no access to the outside internet at all (so no ELPA or
MELPA updates).  Hence, keeping only a small number of external dependencies
under <code>elpa/</code> maximizes portability and maintainability between systems.
</p>

<p>
Altogether, I wind up using Emacs 29+ on all three of the major platforms, in
both GUI and TTY mode.  So this config is designed to work equally well for:
</p>

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">platform</th>
<th scope="col" class="org-left">terminal</th>
<th scope="col" class="org-left">GUI</th>
<th scope="col" class="org-left">ssh + TTY</th>
<th scope="col" class="org-left">Tramp</th>
</tr>
</thead>
<tbody>
<tr>
<td class="org-left">Linux</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
</tr>

<tr>
<td class="org-left">macOS</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
<td class="org-left">✅</td>
</tr>

<tr>
<td class="org-left">Windows</td>
<td class="org-left">❌</td>
<td class="org-left">✅</td>
<td class="org-left">❌</td>
<td class="org-left">✅</td>
</tr>
</tbody>
</table>
</div>
</div>

<div id="outline-container-org75d1ce9" class="outline-2">
<h2 id="org75d1ce9">Notable Features</h2>
<div class="outline-text-2" id="text-org75d1ce9">
<p>
You may notice that despite the laudable goal of intended minimalism, this
document is is still quite long, as I have found many (ever increasing) quirky
behaviors of Emacs that I tweak.  Most of my time is spent in Org, SQL, Python,
Bash, YAML, TOML, and Markdown, so the majority of configuration lies around
these sections.
</p>

<p>
I do make changes to things that I feel "should have been included."  Some
examples of this are:
</p>

<ol class="org-ol">
<li>Additional major modes for common filetypes like Markdown, CSV, and YAML</li>
<li>Error message support for <code>pyright</code> in a <code>*Compilation*</code> buffer</li>
<li>Reasonable indentation behavior for SQL files</li>
<li>Updating buffers automatically if their contents change on disk</li>
<li>Syntax highlighting for Source blocks in Markdown</li>
<li>Handling ANSI color escape codes in shell output, compilation, and VC buffers</li>
</ol>
</div>
</div>

<div id="outline-container-orgb197653" class="outline-2">
<h2 id="orgb197653">Tangling</h2>
<div class="outline-text-2" id="text-orgb197653">
<p>
My configuration is a single literate programming document, which is tangled
into the standard <code>init.el</code> and supporting files.  This is so I can keep track of
all the crazy things I try, and explain them inline with the final code I decide
to include.  Some platforms like GitHub can render this document in a limited
way, but to see all the final configuration values I use you will likely have to
view this document in Emacs itself.
</p>

<p>
Why use a literate document for my configuration?  Basically, as I added more
comments and reminders about what some line of code was doing, where I got it
from, and why it might be commented out, the prose grew longer than the actual
code, and so a change of medium felt prudent.  In my case, that's the venerable
<a href="https://orgmode.org/">Org mode</a>, which comes with Emacs and serves as a way to seamlessly weave
commentary and code together.
</p>
</div>
</div>

<div id="outline-container-org0ce0eb4" class="outline-2">
<h2 id="org0ce0eb4">Inspirations</h2>
<div class="outline-text-2" id="text-org0ce0eb4">
<p>
I steal quite a lot from other, more qualified Emacs community contributors,
such as:
</p>

<ul class="org-ul">
<li><a href="https://protesilaos.com/">Protesilaos Stavrou</a></li>
<li><a href="https://panadestein.github.io/emacsd/">Ramón Panadestein</a></li>
<li><a href="https://www.masteringemacs.org/">Mickey Petersen</a></li>
<li><a href="https://github.com/minad">Daniel Mendler</a></li>
<li><a href="https://github.com/oantolin">Omar Antolín Camarena</a></li>
<li><a href="https://www.lucacambiaghi.com/vanilla-emacs/readme.html">Luca's Literate Config</a></li>
</ul>
</div>
</div>

<div id="outline-container-org627339f" class="outline-2">
<h2 id="org627339f">Getting Emacs</h2>
<div class="outline-text-2" id="text-org627339f">
<p>
For a while I would try to compile Emacs myself, but installing the whole
compilation toolchain hasn't been worth it lately, especially on Windows.
Instead, I've started simply downloading emacs from these sources on each of the
platforms:
</p>
</div>

<div id="outline-container-org547483b" class="outline-3">
<h3 id="org547483b">Windows</h3>
<div class="outline-text-3" id="text-org547483b">
<p>
I go to the <a href="https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-29/">pretest FTP</a> to get the latest version of Emacs.  Usually not quite
up-to-date with the master branch, but still one version number ahead of the
most recent official release.
</p>
</div>
</div>

<div id="outline-container-org89a7587" class="outline-3">
<h3 id="org89a7587">Mac</h3>
<div class="outline-text-3" id="text-org89a7587">
<p>
On macOS, I've had the best luck with <a href="https://github.com/jimeh/emacs-builds/releases">jimeh's nightly builds</a>.  These Emacs.app
bundles have no external dependencies, signed with a developer certificate, and
notarized by Apple, so it <i>just works</i>.  Even without administrator permissions,
you can drag the bundle to the "Applications" folder under your user home
instead, and Emacs still works beautifully.
</p>

<p>
In particular, this feature has saved me a lot of headaches that I ran into
compiling Emacs on my own:
</p>

<blockquote>
<p>
Emacs.app is signed with a developer certificate and notarized by Apple.
</p>
</blockquote>

<p>
Very nice!
</p>
</div>
</div>

<div id="outline-container-orge9d5ff1" class="outline-3">
<h3 id="orge9d5ff1">Linux</h3>
<div class="outline-text-3" id="text-orge9d5ff1">
<p>
Depending on the machine, I get Emacs one of several ways in a GNU/Linux setup.
These rank from highest to lowest priority:
</p>

<ol class="org-ol">
<li>Through my system package manager, such as <code>sudo apt-get install emacs</code> or <code>pacman -S emacs</code></li>
<li>Through the <a href="https://ftp.gnu.org/gnu/emacs/">official FTP</a></li>
<li>Through the <a href="https://alpha.gnu.org/gnu/emacs/pretest/windows/emacs-29/">pretest FTP</a></li>
<li>Through <a href="https://github.com/mariusvniekerk/condax">condax</a></li>
<li>Compiling it myself</li>
</ol>
</div>

<div id="outline-container-org13e9285" class="outline-4">
<h4 id="org13e9285">Compiling</h4>
<div class="outline-text-4" id="text-org13e9285">
<p>
If I do ever want to compile it myself, these are the options I use, making sure
to export the correct <code>CC</code> and <code>GCC</code> variables:
</p>

<div class="org-src-container">
<pre class="src src-shell">git clone git://git.savannah.gnu.org/emacs.git --branch emacs-29 --depth 1
export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10
./autogen.sh
./configure \
  --prefix=/c/emacs-29 \
  --with-native-compilation \
  --with-tree-sitter \
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
make --jobs=$(nproc)
sudo make install
</pre>
</div>
</div>
</div>
</div>
</div>

<div id="outline-container-orgbf3faba" class="outline-2">
<h2 id="orgbf3faba">Header</h2>
<div class="outline-text-2" id="text-orgbf3faba">
<p>
To comply with the Emacs <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Library-Headers.html">conventions for libraries</a>, the tangled init.el must
have the following header and <a href="#org95eef66">footer:</a>
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">;;; init.el --- Robb's Emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2022 Robert Enzmann

;; Author: Robb Enzmann &lt;robbenzmann@gmail.com&gt;
;; Keywords: internal
;; URL: https://robbmann.io/

;;; Commentary:
;; A mostly minimal, reproducible Emacs configuration.  This file is
;; automatically tangled from README.org, with header/footer comments on each
;; code block that allow for de-tangling the source back to README.org when
;; working on this file directly.

;;; Code:
</pre>
</div>
</div>
</div>

<div id="outline-container-org5acf5d4" class="outline-2">
<h2 id="org5acf5d4">Custom</h2>
<div class="outline-text-2" id="text-org5acf5d4">
<p>
I prefer having <code>custom</code> modify its own file.  This next snippet ensures any
<code>package-install</code> or <code>custom</code> edits go to <code>custom.el</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgaed21b5" class="outline-2">
<h2 id="orgaed21b5">Proxy settings</h2>
<div class="outline-text-2" id="text-orgaed21b5">
<p>
When behind a corporate proxy, we might have to authenticate before we can pull
packages off ELPA.  Emacs only uses the HOST and PORT portions of the
<code>http_proxy</code> and <code>https_proxy</code> environment variables, so we need to set LOGIN
(user id) and PASSWORD ourselves.
</p>

<p>
I store the login, port, and host variables in a <code>proxy.el</code> file (obviously
outside version control) when I'm on a machine that's behind an http proxy.  We
grab the password interactively when such a file exists.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/enable-proxy ()
  (interactive)
  "Turn on HTTP proxy."
  (let ((proxy-file (expand-file-name "proxy.el" user-emacs-directory)))
    (when (file-exists-p proxy-file)
      (load-file proxy-file)
      (setq url-proxy-services
            `(("no_proxy" . "^\\(localhost\\|10.*\\)")
              ("http" . ,(concat renz/proxy-host ":" renz/proxy-port))
              ("https" . ,(concat renz/proxy-host ":" renz/proxy-port))))
      (setq url-http-proxy-basic-auth-storage
            (list
             (list
              (concat renz/proxy-host ":" renz/proxy-port)
              (cons renz/proxy-login
                    (base64-encode-string
                     (concat renz/proxy-login ":" (password-read "Proxy password: "))))))))))
</pre>
</div>
</div>
</div>

<div id="outline-container-org31e6402" class="outline-2">
<h2 id="org31e6402">Packages</h2>
<div class="outline-text-2" id="text-org31e6402">
<p>
The initial cornerstone of every Emacs configuration is a decision on package
management and configuration.  I opt for <code>use-package</code> and <code>package.el</code>, since both
are built-in to Emacs 29+, which helps maximize stability and portability.
</p>

<p>
To avoid loading packages twice, <a href="https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html">the manual</a> recommends disabling
<code>package-enable-at-startup</code> in <code>init.el</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
</pre>
</div>

<p>
I do not use the <code>:ensure t</code> keyword in <code>use-package</code> declarations to install
packages, because I cannot always ensure that I have a stable connection to GNU
ELPA (in the case of <code>package-install-selected-packages</code>) or the public
<code>github.com</code> (for <code>package-vc-install-selected-packages</code>).  Instead, I rely on
<code>M-x package-install</code> and <code>M-x package-delete</code>, and only permit <code>use-package</code> to
handle the configuration and loading of packages.  As mentioned in the
introduction, each package's source is explicitly included into version control
of my configuration, so I don't worry too much about pinning package versions in
this file.  When I want to update a package, I use <code>M-x package-update</code>, the
<code>package.el</code> user interface, or delete the package's source folder and use
<code>renz/package-sync</code> (defined below).  Should something go wrong, I roll back to
a previous commit.  So far, this method has been reliable for keeping my
<code>init.el</code> (this README), <code>custom.el</code>, the <code>package-selected-packages</code> variable,
and <code>elpa/</code> directory all in sync with one another.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/package-sync ()
  "Remove unused sources and install any missing ones."
  (interactive)
  (package-autoremove)
  (package-install-selected-packages)
  (package-vc-install-selected-packages))

(when (and (cl-notevery 'package-installed-p package-selected-packages)
           (yes-or-no-p "Install VC packages?"))
  (package-vc-install-selected-packages))
</pre>
</div>

<p>
There are also a few hand-made packages I keep around in a special
<code>.emacs.d/site-lisp</code> directory.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-to-list 'load-path (expand-file-name "site-lisp/" user-emacs-directory))
</pre>
</div>
</div>
</div>

<div id="outline-container-org76d40d4" class="outline-2">
<h2 id="org76d40d4">OS-specific Configuration</h2>
<div class="outline-text-2" id="text-org76d40d4">
</div>
<div id="outline-container-org4d7e63c" class="outline-3">
<h3 id="org4d7e63c">Microsoft Windows</h3>
<div class="outline-text-3" id="text-org4d7e63c">
<p>
Windows, funnily enough, has some trouble registering the Windows key as a
usable modifier for Emacs.  In fact, <code>s-l</code> will <i>never</i> be an option, since it's
handled at the hardware level.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/windowsp ()
  "Are we on Microsoft Windows?"
  (memq system-type '(windows-nt cygwin ms-dos)))
</pre>
</div>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(when (and (renz/windowsp) (executable-find "pwsh"))
  (setq shell-file-name "pwsh"))
</pre>
</div>

<p>
There are a few things I set up independent of Emacs.  Namely, <code>find</code>, <code>xargs</code>, and
<code>rg</code>.  These days, I can usually install these things with <code>winget</code>:
</p>

<div class="org-src-container">
<pre class="src src-shell">winget install BurntSushi.ripgrep.GNU
winget install GnuWin32.FindUtils
winget install GnuWin32.Grep
winget install RubyInstallerTeam.RubyWithDevKit.3.2  # For building my website with Jekyll
winget install Python.Python.3.11  # I work a lot in python
</pre>
</div>

<p>
You can use Emacs without these, but some commands like <code>M-x grep</code> or <code>M-x
project-find-regexp</code> will not work without making sure the GNU version of <code>find</code>
and <code>grep</code> (or a suitable replacement) are on your PATH.  I tend not to muck with
PATH inside Emacs if I can help it, and instead launch Emacs from powershell
where things are properly set.  Usually I'll have some things like this in my <code>$PROFILE</code>:
</p>

<div class="org-src-container">
<pre class="src src-powershell">$ENV:Path = "${ENV:ProgramFiles}\Hunspell\bin\;" + $ENV:Path
$ENV:Path = "${ENV:ProgramFiles(x86)}\GnuWin32\bin\;" + $ENV:Path
$ENV:Path = "${ENV:ProgramFiles}\Emacs\emacs-29.1\bin\;" + $ENV:Path
$ENV:PROFILE = $PROFILE
$ENV:LANG = "en_US"
$ENV:DICPATH = "$ENV:ProgramFiles\Hunspell\"
</pre>
</div>

<p>
The duplicate <code>$PROFILE</code> thing is so we can access that file through <code>C-x C-f
$PROFILE</code> within Emacs.  Check out the <a href="#org0f9cda5">Spellchecking</a> section on the Hunspell
stuff.
</p>
</div>

<div id="outline-container-orge32e140" class="outline-4">
<h4 id="orge32e140">On the winkey</h4>
<div class="outline-text-4" id="text-orge32e140">
<p>
For a time I considered enabling the use of the winkey like this:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key
(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key
</pre>
</div>

<p>
Followed by enabling specific chords, such as "winkey+a":
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(w32-register-hot-key [s-a])
</pre>
</div>

<p>
Since I've taken a more TTY-friendly approach for my config in general, where
super can be a bit tough to integrate with both the windowing application <i>and</i>
the terminal emulator, I've mostly given up on the GUI key in favor of other
chords, especially the <code>C-c</code> ones.
</p>
</div>
</div>
</div>

<div id="outline-container-org401af32" class="outline-3">
<h3 id="org401af32">macOS</h3>
<div class="outline-text-3" id="text-org401af32">
<p>
Launching Emacs from the typical application launcher or command-space usually
won't capture any modifications to <code>$PATH</code>, typically handled in a file like
<code>~/.profile</code> or <code>~/.bashrc</code>. So, the main configuration included here is from
<a href="https://github.com/purcell/exec-path-from-shell">exec-path-from-shell</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(when (eq system-type 'darwin)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org4934940" class="outline-2">
<h2 id="org4934940">Font</h2>
<div class="outline-text-2" id="text-org4934940">
<p>
Fonts are a tricky business.  See Emacs/Fonts in the manual (<code>C-h i</code>) for relevant
information on how checking and setting default fonts works:
</p>

<ul class="org-ul">
<li><a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lookup.html">Searching for installed fonts</a></li>
<li><a href="https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html">Setting the default font</a></li>
</ul>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(cond ((x-list-fonts "Hack Nerd Font")
       (add-to-list 'default-frame-alist '(font . "Hack Nerd Font-12")))
      ;; ((x-list-fonts "Segoe UI Emoji")
      ;;  (add-to-list 'default-frame-alist '(font . "Segoe UI Emoji-12")))
      )

(defun renz/change-font-size (new-size)
  "Change the default font size to the given size."
  (interactive "nNew font size: ")
  (set-face-attribute 'default nil :height (* 10 new-size)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org4781c04" class="outline-2">
<h2 id="org4781c04">Theme</h2>
<div class="outline-text-2" id="text-org4781c04">
<p>
With the introduction of <code>modus-vivendi-tinted</code> in Emacs 29, I really have no need for any
external themes now.  It is accessible, well optimized for <code>org-mode</code> and
<code>prog-mode</code>, and distributed with vanilla Emacs.  Hats off to <a href="https://protesilaos.com/">Prot</a> for these
wonderful themes.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package emacs
  :custom
  (modus-themes-inhibit-reload nil)
  (modus-themes-subtle-line-numbers t)
  (modus-themes-syntax '(alt-syntax faint green-strings yellow-comments))
  (modus-themes-diffs 'desaturated)
  ;; (modus-themes-hl-line 'intense)
  (modus-themes-deuteranopia nil)
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  ;; (modus-themes-mode-line 'borderless)
  (modus-themes-fringes 'subtle)
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-vivendi-color-overrides '((bg-main . "#010101")))
  :bind   ("&lt;f5&gt;" . modus-themes-toggle)
  :config
  (if (version&lt; emacs-version "29.0")
      (load-theme 'leuven-dark)
    (load-theme 'modus-vivendi-tinted t)))
</pre>
</div>
</div>
</div>

<div id="outline-container-orga321a2d" class="outline-2">
<h2 id="orga321a2d">Emacs' Built-in Settings</h2>
<div class="outline-text-2" id="text-orga321a2d">
<p>
My settings for base Emacs behavior.  Assuming I ran with <i>no</i> plugins (ala <code>emacs
-Q</code>), I would still set most of these by hand at one point or another.  This
section is designed for variables that modify Emacs and its editing behavior
directly.  Configuation for built-in tools, such as Dired, Tramp, and
Tree-sitter are located under <a href="#orgf8e4410">Tool configuration</a>.
</p>
</div>

<div id="outline-container-orgb632d92" class="outline-3">
<h3 id="orgb632d92">Stop stupid bell</h3>
<div class="outline-text-3" id="text-orgb632d92">
<p>
This snippet has a special place in my heart, because it was the first two lines
of elisp I wrote when first learning Emacs.  It is the central kernel around
which my <code>~/.emacs</code> and later <code>~/.emacs.d/init.el</code> grew.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">;; Stop stupid bell
(setq ring-bell-function 'ignore)
</pre>
</div>

<p>
The bell is really, <i>really</i> annoying.
</p>
</div>
</div>

<div id="outline-container-org85bf3b5" class="outline-3">
<h3 id="org85bf3b5">Start a server for <code>emacsclient</code></h3>
<div class="outline-text-3" id="text-org85bf3b5">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(server-start)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgb3a0b92" class="outline-3">
<h3 id="orgb3a0b92">Don't hang when visiting files with extremely long lines</h3>
<div class="outline-text-3" id="text-orgb3a0b92">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-so-long-mode t)
</pre>
</div>
</div>
</div>

<div id="outline-container-orga557a67" class="outline-3">
<h3 id="orga557a67">Unicode</h3>
<div class="outline-text-3" id="text-orga557a67">
<p>
Sometimes (especially on Windows), Emacs gets confused about what encoding to
use.  These settings try to prevent that confusion.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgd23b347" class="outline-3">
<h3 id="orgd23b347">Mode line</h3>
<div class="outline-text-3" id="text-orgd23b347">
<p>
It's easy for the mode line to get cluttered once things like Flymake and eglot
kick in.  When I was starting out, I used to have these two settings:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq display-battery-mode t
      display-time-day-and-date t)

(display-time)
</pre>
</div>

<p>
After a while I noticed that I'm almost never running Emacs in a full screen
where I can't see the battery or date in the corner of my window manager, so
they were just wasting mode line space.  Nowadays I simply opt for column mode
and a dimmed mode line in non-selected windows.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq column-number-mode t
      mode-line-in-non-selected-windows t)
</pre>
</div>
</div>
</div>

<div id="outline-container-org6a89cbd" class="outline-3">
<h3 id="org6a89cbd">Remember minibuffer history</h3>
<div class="outline-text-3" id="text-org6a89cbd">
<p>
Found this on a <a href="https://www.youtube.com/watch?v=51eSeqcaikM">System Crafters video</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq history-length 25)
(savehist-mode 1)
</pre>
</div>
</div>
</div>

<div id="outline-container-org5749f41" class="outline-3">
<h3 id="org5749f41">Render ASCII color escape codes</h3>
<div class="outline-text-3" id="text-org5749f41">
<p>
For files containing color escape codes, this provides a way to render the
colors in-buffer.  Provided by a <a href="https://stackoverflow.com/a/3072831/13215205">helpful stackoverflow answer</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/display-ansi-colors ()
  "Render colors in a buffer that contains ASCII color escape codes."
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))
</pre>
</div>
</div>

<div id="outline-container-orgaabac93" class="outline-4">
<h4 id="orgaabac93">Colored output in <code>eshell</code> and <code>*compilation*</code></h4>
<div class="outline-text-4" id="text-orgaabac93">
<p>
In <code>*compilation*</code> mode, we just use the "display colors" function from above.
Enable colors in the <code>*compilation*</code> buffer.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'compilation-filter-hook #'renz/display-ansi-colors)
</pre>
</div>

<p>
For <code>eshell</code>, this is copy-pasted from a <a href="https://emacs.stackexchange.com/questions/9517/colored-git-output-in-eshell">stack overflow question</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'eshell-preoutput-filter-functions  #'ansi-color-apply)
</pre>
</div>
</div>
</div>

<div id="outline-container-org7c5ed8e" class="outline-4">
<h4 id="org7c5ed8e">xterm-color</h4>
<div class="outline-text-4" id="text-org7c5ed8e">
<p>
Soon, I'd like to swap out my hacks above for this more robust package:
<a href="https://github.com/atomontage/xterm-color/tree/master">https://github.com/atomontage/xterm-color/tree/master</a>
</p>
</div>
</div>
</div>

<div id="outline-container-orgb18591b" class="outline-3">
<h3 id="orgb18591b">Recent files menu</h3>
<div class="outline-text-3" id="text-orgb18591b">
<p>
This enables "File -&gt; Open Recent" from the menu bar and using <code>completing-read</code> over the <code>recentf-list</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(recentf-mode t)

(defun renz/find-recent-file ()
  "Find a file that was recently visted using `completing-read'."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list nil t)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org34e2a1a" class="outline-3">
<h3 id="org34e2a1a">Fill-column</h3>
<div class="outline-text-3" id="text-org34e2a1a">
<p>
Regardless of whether we're doing visual fill or hard fill, I like the default
at around 80 characters, and I'll manually change it per buffer if I want
something different
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq-default fill-column 80)
</pre>
</div>
</div>
</div>

<div id="outline-container-orge2406d7" class="outline-3">
<h3 id="orge2406d7">Scroll bar</h3>
<div class="outline-text-3" id="text-orge2406d7">
<p>
I toggle this one on/off sometimes depending on how I feel and which OS I'm
currently on.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(scroll-bar-mode -1)
</pre>
</div>

<p>
By default, though, I prefer it to be off when I start Emacs.
</p>
</div>
</div>

<div id="outline-container-org94fb80d" class="outline-3">
<h3 id="org94fb80d">Window margins and fringe</h3>
<div class="outline-text-3" id="text-org94fb80d">
<p>
This hunk adds some space around all sides of each window so that we get a clear
space between the edge of the screen and the fringe.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/modify-margins ()
  "Add some space around each window."
  (interactive)
  (modify-all-frames-parameters
   '((right-divider-width . 40)
     (internal-border-width . 40)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background)))

(renz/modify-margins)
</pre>
</div>

<p>
We also need to make sure this runs each time we change the <code>ef-theme</code>, otherwise
the old background color will linger in the margins.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'ef-themes-post-load-hook 'renz/modify-margins)
</pre>
</div>
</div>
</div>

<div id="outline-container-org84f9dd0" class="outline-3">
<h3 id="org84f9dd0">Automatically visit symlink sources</h3>
<div class="outline-text-3" id="text-org84f9dd0">
<p>
When navigating to a file that is a symlink, this automatically redirects us to
the source file it's pointing to.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq find-file-visit-truename t)
(setq vc-follow-symlinks t)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgaa0a88d" class="outline-3">
<h3 id="orgaa0a88d">Indent with spaces by default</h3>
<div class="outline-text-3" id="text-orgaa0a88d">
<p>
For the most part I edit Python, SQL, Markdown, Org, and shell scripts.  All of
these favor spaces over tabs, so I prefer this as the default.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq-default indent-tabs-mode nil)
</pre>
</div>

<p>
Generally, though, indentation behavior is set by major-mode functions, which
may or may not use Emacs' built-in indentation functions.  For instance, when
trying to find the functions behind indentation in shell mode, I came across
<code>smie.el</code>, whose introductory comments include this gem:
</p>

<blockquote>
<p>
OTOH we had to kill many chickens, read many coffee grounds, and practice
untold numbers of black magic spells, to come up with the indentation code.
Since then, some of that code has been beaten into submission, but the
`smie-indent-keyword' function is still pretty obscure.
</p>
</blockquote>

<p>
Even the <a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto_002dIndentation.html">GNU manual</a> speaks of it in the same way:
</p>

<blockquote>
<p>
Writing a good indentation function can be difficult and to a large extent it is
still a black art. Many major mode authors will start by writing a simple
indentation function that works for simple cases, for example by comparing with
the indentation of the previous text line. For most programming languages that
are not really line-based, this tends to scale very poorly: improving such a
function to let it handle more diverse situations tends to become more and more
difficult, resulting in the end with a large, complex, unmaintainable
indentation function which nobody dares to touch.
</p>
</blockquote>
</div>
</div>

<div id="outline-container-org5aa0afa" class="outline-3">
<h3 id="org5aa0afa">Enable horizontal scrolling with mouse</h3>
<div class="outline-text-3" id="text-org5aa0afa">
<p>
From a helpful <a href="https://stackoverflow.com/a/67758169">stackoverflow answer.</a>
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq mouse-wheel-tilt-scroll t)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgd051027" class="outline-3">
<h3 id="orgd051027">Window management</h3>
<div class="outline-text-3" id="text-orgd051027">
<p>
From a Mickey Petersen <a href="https://www.masteringemacs.org/article/demystifying-emacs-window-manager">article</a>, this causes <code>switch-to-buffer</code> to open the
selected buffer in the current window rather than switching windows, assuming
both are open in the current frame.  This is more frequently the behavior I
intend when I'm trying to get a window to display a specific buffer.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(unless (version&lt; emacs-version "27.1")
  (setq switch-to-buffer-obey-display-actions t))
</pre>
</div>
</div>
</div>

<div id="outline-container-org106045c" class="outline-3">
<h3 id="org106045c">Automatically update buffers when contents change on disk</h3>
<div class="outline-text-3" id="text-org106045c">
<p>
Without setting <code>global-auto-revert-mode</code>, we have to remember to issue a
<code>revert-buffer</code> or <code>revert-buffer-quick</code> (<code>C-x x g</code> by default) in case a file
changed.  Over Tramp, we still have to manually revert files when they've
changed on disk.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-auto-revert-mode)
</pre>
</div>
</div>
</div>

<div id="outline-container-org27c4054" class="outline-3">
<h3 id="org27c4054">Highlight the line point is on</h3>
<div class="outline-text-3" id="text-org27c4054">
<p>
Add a faint background highlight to the line we're editing.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook #'hl-line-mode)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc003831" class="outline-3">
<h3 id="orgc003831">Always turn on flymake in prog mode</h3>
<div class="outline-text-3" id="text-orgc003831">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'prog-mode-hook #'flymake-mode)
</pre>
</div>

<p>
Another, related mode is <code>flyspell-prog-mode</code>, which is just checks spelling in
comments and strings.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'prog-mode-hook #'flyspell-prog-mode)
</pre>
</div>
</div>
</div>

<div id="outline-container-org0b08041" class="outline-3">
<h3 id="org0b08041">Automatically create matching parens in programming modes</h3>
<div class="outline-text-3" id="text-org0b08041">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'prog-mode-hook (electric-pair-mode t))
(add-hook 'prog-mode-hook (show-paren-mode t))
</pre>
</div>
</div>
</div>

<div id="outline-container-org2197d50" class="outline-3">
<h3 id="org2197d50">Shorten yes/no prompts to y/n</h3>
<div class="outline-text-3" id="text-org2197d50">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq use-short-answers t)
</pre>
</div>
</div>
</div>

<div id="outline-container-org94d7908" class="outline-3">
<h3 id="org94d7908">Delete whitespace on save</h3>
<div class="outline-text-3" id="text-org94d7908">
<p>
I would also like to have a good-looking display for trailing whitespace and
leading tabs like in my Neovim setup, but it has proven challenging to just
narrow down to those two faces.  In the interim, I toggle <code>M-x whitespace-mode</code> to
check for mixed tabs, spaces, and line endings.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'before-save-hook 'delete-trailing-whitespace)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgaaac725" class="outline-3">
<h3 id="orgaaac725">Killing buffers with a running process</h3>
<div class="outline-text-3" id="text-orgaaac725">
<p>
Typically, Emacs will ask you to confirm before killing a buffer that has a
running process, such as with <code>run-python</code>, a <code>*shell*</code> buffer, or a <code>*compilation*</code>
buffer.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq kill-buffer-query-functions
  (remq 'process-kill-buffer-query-function
         kill-buffer-query-functions))
</pre>
</div>
</div>
</div>

<div id="outline-container-org05dc77e" class="outline-3">
<h3 id="org05dc77e">Don't wrap lines</h3>
<div class="outline-text-3" id="text-org05dc77e">
<p>
I much prefer having long lines simply spill off to the right of the screen than
having them wrap around onto the next line, except in the case where I'd like to
see wrapped line content, like in one of the shell modes.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq-default truncate-lines t)
(add-hook 'eshell-mode-hook (lambda () (setq-local truncate-lines nil)))
(add-hook 'shell-mode-hook (lambda () (setq-local truncate-lines nil)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org7cf2629" class="outline-3">
<h3 id="org7cf2629">Relative line numbers</h3>
<div class="outline-text-3" id="text-org7cf2629">
<p>
For programming and prose/writing modes.
</p>

<p>
Unfortunately, line numbers are displayed in the text area of the buffer, but
org-modern uses the fringe to display source blocks.  <a href="https://www.reddit.com/r/emacs/comments/ymprwi/comment/iv5iafb/?utm_source=share&amp;utm_medium=web2x&amp;context=3">There's no way to display
them to the left</a> of the fringe, so I'm careful about only turning on line
numbers in modes that I think I'll benefit from it.  It's been working pretty
well in org-mode without the line numbers so far, since for each of the code
blocks I can always use <code>C-c '</code> to edit in <code>prog-mode</code>, where I <i>do</i> get line numbers.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/display-relative-lines ()
  (setq display-line-numbers 'relative))

(add-hook 'prog-mode-hook #'renz/display-relative-lines)
(add-hook 'yaml-mode-hook #'renz/display-relative-lines)

(unless (display-graphic-p)
  (add-hook 'text-mode-hook #'renz/display-relative-lines))
</pre>
</div>
</div>
</div>

<div id="outline-container-org62c6dc5" class="outline-3">
<h3 id="org62c6dc5">Delete region when we yank on top of it</h3>
<div class="outline-text-3" id="text-org62c6dc5">
<p>
I just think that's a funny sentence.  Normally when yanking text with an active
region, the region will remain and the yanked text is just inserted at point.  I
prefer the modern word processor behavior of replacing the selected text with
the yanked content.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(delete-selection-mode t)
</pre>
</div>
</div>
</div>

<div id="outline-container-org9cc4b57" class="outline-3">
<h3 id="org9cc4b57">Enable mouse in terminal/TTY</h3>
<div class="outline-text-3" id="text-org9cc4b57">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(xterm-mouse-mode 1)
</pre>
</div>
</div>
</div>

<div id="outline-container-org984157a" class="outline-3">
<h3 id="org984157a">Compilation</h3>
<div class="outline-text-3" id="text-org984157a">
<p>
As new text appears, the default behavior is for it to spill off the bottom,
unless we manually scroll to the end of the buffer.  Instead, I prefer the
window to automatically scroll along with text as it appears, stopping at the
first error that appears.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq compilation-scroll-output 'first-error)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgeea5753" class="outline-3">
<h3 id="orgeea5753">Tool bar</h3>
<div class="outline-text-3" id="text-orgeea5753">
<p>
I usually leave the tool bar disabled
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(tool-bar-mode -1)
</pre>
</div>

<p>
The <i>menu</i> bar, on the other hand <code>(menu-bar-mode)</code>, is very handy, and I only
disable it on Windows, where it looks hideous if I'm running in dark mode.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(when (renz/windowsp)
  (menu-bar-mode -1))
</pre>
</div>

<p>
For newcomers to Emacs, I would strongly discourage disabling the menu bar, as
it is the most straightforward way to discover Emacs' most useful features.
</p>
</div>
</div>

<div id="outline-container-org7b6a7de" class="outline-3">
<h3 id="org7b6a7de">Ignore risky .dir-locals.el</h3>
<div class="outline-text-3" id="text-org7b6a7de">
<p>
From an <a href="https://emacs.stackexchange.com/a/44604">Emacs stackexchange</a> answer.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(advice-add 'risky-local-variable-p :override #'ignore)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgfba504c" class="outline-3">
<h3 id="orgfba504c">Prefer <code>rg</code> over <code>grep</code></h3>
<div class="outline-text-3" id="text-orgfba504c">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package grep
  :config
  (when (executable-find "rg")
    (setq grep-program "rg")
    (grep-apply-setting
     'grep-find-command
     '("rg -n -H --color always --no-heading -e '' $(git rev-parse --show-toplevel || pwd)" . 27))))
</pre>
</div>

<p>
If you're on Windows, this command assumes you're running <code>pwsh</code> version 7 or higher.
</p>
</div>
</div>

<div id="outline-container-org5a6f171" class="outline-3">
<h3 id="org5a6f171">Shorter file paths in grep/compilation buffers</h3>
<div class="outline-text-3" id="text-org5a6f171">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package scf-mode
  :load-path "site-lisp"
  :hook (grep-mode . (lambda () (scf-mode 1))))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgea974f0" class="outline-3">
<h3 id="orgea974f0">Confirm when exiting Emacs</h3>
<div class="outline-text-3" id="text-orgea974f0">
<p>
It's very annoying when I'm working and suddenly I meant to do <code>C-c C-x</code>, but
instead hit <code>C-x C-c</code>.  This helps prevent that.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq confirm-kill-emacs 'yes-or-no-p)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgd57c9b5" class="outline-3">
<h3 id="orgd57c9b5">Smooth scrolling</h3>
<div class="outline-text-3" id="text-orgd57c9b5">
<p>
Emacs 29 introduced smooth, pixel-level scrolling, which removes much of the
"jumpiness" you see when scrolling past images.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(if (version&lt; emacs-version "29.0")
    (pixel-scroll-mode)
  (pixel-scroll-precision-mode 1)
  (setq pixel-scroll-precision-large-scroll-height 35.0))
</pre>
</div>
</div>
</div>

<div id="outline-container-org0f9cda5" class="outline-3">
<h3 id="org0f9cda5">Spellchecking</h3>
<div class="outline-text-3" id="text-org0f9cda5">
<p>
On macOS and linux I typically use <code>aspell</code>, given how easy it is to install.  For
Windows, I'll set up <a href="http://hunspell.github.io/">hunspell</a>, which I install from <a href="https://github.com/iquiw/hunspell-binary/releases/">the hunspell-binary repo</a>.
After installing the <code>hunspell</code> binary, it requires installing a dictionary and
affix file to the installation directory:
</p>

<div class="org-src-container">
<pre class="src src-shell">curl -o en_US.dic https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic?id=a4473e06b56bfe35187e302754f6baaa8d75e54f
curl -o en_US.aff https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff?id=a4473e06b56bfe35187e302754f6baaa8d75e54f
</pre>
</div>

<p>
Then move these files to wherever hunspell is.  For instance, <code>C:\Program Files\Hunspell</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(cond ((executable-find "aspell")
       (setq ispell-program-name "aspell"
             ispell-really-aspell t))
      ((executable-find "hunspell")
       (setq ispell-program-name "hunspell"
             ispell-really-hunspell t)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org5b3698d" class="outline-3">
<h3 id="org5b3698d">Backup and auto-save files</h3>
<div class="outline-text-3" id="text-org5b3698d">
<p>
Keep all backup files in a temporary folder.  At the moment I have some "file
not found" errors popping up during auto-save on Windows.  Once I debug that,
I'll uncomment the second part.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq backup-directory-alist '(("." . "~/.emacs.d/backups/"))
      ;; auto-save-file-name-transforms '(("." ,temporary-file-directory t))
      )
</pre>
</div>
</div>
</div>

<div id="outline-container-orgf6c18f0" class="outline-3">
<h3 id="orgf6c18f0">Enable <code>narrow-to-region</code></h3>
<div class="outline-text-3" id="text-orgf6c18f0">
<p>
<code>narrow-to-region</code> restricts editing in this buffer to the current region.  The
rest of the text becomes temporarily invisible and untouchable but is not
deleted; if you save the buffer in a file, the invisible text is included in the
file.  <code>C-x n w</code> makes all visible again.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(put 'narrow-to-region 'disabled nil)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgf8bcac2" class="outline-3">
<h3 id="orgf8bcac2">Enable up/downcase-region</h3>
<div class="outline-text-3" id="text-orgf8bcac2">
<p>
Allows us to convert entire regions to upper or lower case.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgfe8224a" class="outline-3">
<h3 id="orgfe8224a">Mark rings and registers: bigger, faster, stronger</h3>
<div class="outline-text-3" id="text-orgfe8224a">
<p>
16 is the default number of marks stored on the global and local mark rings
is 16.  I hop around much more than 16 times as I'm editing, so I expand this a
bit.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq-default mark-ring-max 32)
(setq global-mark-ring-max 32)
</pre>
</div>

<p>
Another handy shortcut is continually popping marks by repeated <code>C-&lt;SPC&gt;</code> after
the first <code>C-u C-&lt;SPC&gt;</code> through the <code>set-mark-command-repeat-pop</code> setting.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq set-mark-command-repeat-pop t)
</pre>
</div>

<p>
And, because I always forget it, to pop a global mark you use <code>C-x C-&lt;SPC&gt;</code>.  The
local version, <code>C-u C-&lt;SPC&gt;</code> will only pop marks from the current buffer.  So the
<code>C-x C-&lt;SPC&gt;</code> version is much closer to how Vim's jump stack works.
</p>

<p>
A handy "bookmark" system (aside from actual bookmarks) is to set common buffers
and files to registers pre-emptively.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(set-register ?S '(buffer . "*scratch*"))
(set-register ?I `(file . ,(expand-file-name "README.org" user-emacs-directory)))
(set-register ?B `(file . "~/.bashrc"))
</pre>
</div>

<p>
The default keybinding for <code>jump-to-register</code> is <code>C-x r j R</code>, where <code>R</code> is the name of
the register.  My own personal convention here is to use lower-case letter for
interactive session bookmarks that will be lost between sessions, and upper-case
letters for ones I've set permanently here.
</p>

<p>
Before I was aware of this feature I had created my own <code>jump-to-X</code> style
functions, but this is much better!  You even get a handy pop-up if you wait a
second after typing <code>C-x r j</code> to see all the available registers.
</p>
</div>
</div>
</div>

<div id="outline-container-org173911a" class="outline-2">
<h2 id="org173911a">Keybindings</h2>
<div class="outline-text-2" id="text-org173911a">
</div>
<div id="outline-container-org6b731d8" class="outline-3">
<h3 id="org6b731d8">Expanded/better defaults</h3>
<div class="outline-text-3" id="text-org6b731d8">
<p>
These convenient chords allow for fast text replacement by holding <code>C-M-</code> and
rapidly typing <code>k</code> and <code>h</code> in succession.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-M-&lt;backspace&gt;") 'backward-kill-sexp)
(global-set-key (kbd "C-M-h") 'backward-kill-sexp)
</pre>
</div>

<p>
The next line UNBINDS the suspend-frame keybinding.  Accidentally minimizing on
the GUI was frustrating as hell, so now I use <code>C-x C-z</code> if I <i>really</i> want to
suspend the frame.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-z") #'zap-up-to-char)
</pre>
</div>

<p>
Hippie-expand <a href="https://www.masteringemacs.org/article/text-expansion-hippie-expand">is purported</a> to be a better version of <code>dabbrev</code>, but I rather like
the default behavior of <code>dabbrev</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key [remap dabbrev-expand] 'hippie-expand)
</pre>
</div>

<p>
<code>ibuffer</code> is a strictly superior, built-in version of its counterpart.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key [remap list-buffers] 'ibuffer)
</pre>
</div>

<p>
The most common situation where I'm running <code>flymake</code> would be for spelling in
prose, or diagnostics from a language server.  In either case, I like having
next/previous on easy to reach chords.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package flymake
  :bind (:map flymake-mode-map
         ("C-c n" . flymake-goto-next-error)
         ("C-c p" . flymake-goto-prev-error)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org407db0b" class="outline-3">
<h3 id="org407db0b">Overriding defaults</h3>
<div class="outline-text-3" id="text-org407db0b">
<p>
Some default bindings aren't useful for me, so I bind them to actions I take
more frequently.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-x C-p") 'previous-buffer)  ; Overrides `mark-page'
(global-set-key (kbd "C-x C-n") 'next-buffer)      ; Overrides `set-goal-column'
</pre>
</div>
</div>
</div>

<div id="outline-container-org159f18a" class="outline-3">
<h3 id="org159f18a">C-c bindings</h3>
<div class="outline-text-3" id="text-org159f18a">
<p>
Emacs has <a href="https://www.gnu.org/software/emacs/manual/html_node/emacs/Key-Bindings.html">some standards</a> about where user-configured keys should go; <code>C-c
&lt;letter&gt;</code> is always free for users.  It may seem like overkill how I set a header
for each possible <code>C-c</code> combination, but it's incredibly handy when I want to jump
directly to one of these headings while in another buffer.  See e.g. <code>org-goto</code>,
which allows me to narrow in on a particular key I'd like to bind by leveraging
<code>completing-read</code>.  If a <code>C-c &lt;letter&gt;</code> combination is missing as a header, then I'm
probably using it in a <code>:bind</code> statement with <code>use-package</code> somewhere else.
</p>
</div>

<div id="outline-container-orgd43ee72" class="outline-4">
<h4 id="orgd43ee72"><code>C-c b</code> build / compile</h4>
<div class="outline-text-4" id="text-orgd43ee72">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c b") #'compile)
(global-set-key (kbd "C-c B") #'recompile)
</pre>
</div>
</div>
</div>

<div id="outline-container-org14280e7" class="outline-4">
<h4 id="org14280e7"><code>C-c c</code> Calendar</h4>
<div class="outline-text-4" id="text-org14280e7">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c c") #'calendar)
</pre>
</div>
</div>
</div>

<div id="outline-container-org0036830" class="outline-4">
<h4 id="org0036830"><code>C-c d</code> Navigating to symbols using old-school TAGS</h4>
<div class="outline-text-4" id="text-org0036830">
<p>
Before the whole language server revolution, we had TAGS files for caching the
location of symbol definitions.  <code>etags</code> comes with Emacs, and combining some
clever use of <code>find</code> with it can render a pretty good symbol search experience.
To generate the TAGS file, I usually have a <code>TAGS</code> recipe that looks something
similar to this in each project's <code>Makefile</code>:
</p>

<div class="org-src-container">
<pre class="src src-shell">find . -type d -name ".venv" -prune \
    -o -type d -name ".ipynb_checkpoints" -prune \
    -o -type d -name ".node_modules" -prune \
    -o -type d -name "elpa" -prune \
    -o -type f -name "*.py" -print \
    -o -type f -name "*.sql" -print \
    -o -type f -name "*.el" -print \
    | etags -
</pre>
</div>

<p>
Then, <code>M-x project-compile RET make TAGS</code> builds a tags table.  At which point, I
can use <code>tags-completion-table</code> to build a list of symbols I can navigate to with
completion, with just a little help from <code>xref-find-definitions</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/find-tag ()
  "Use `completing-read' to navigate to a tag."
  (interactive)
  (require 'etags)
  (tags-completion-table)
  (xref-find-definitions (completing-read "Find tag: " tags-completion-table)))

(global-set-key (kbd "C-c d") #'renz/find-tag)
</pre>
</div>
</div>
</div>

<div id="outline-container-org5b163b7" class="outline-4">
<h4 id="org5b163b7"><code>C-c f</code> find file at point (ffap)</h4>
<div class="outline-text-4" id="text-org5b163b7">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c f") #'ffap)
</pre>
</div>
</div>
</div>

<div id="outline-container-org832f321" class="outline-4">
<h4 id="org832f321"><code>C-c i</code> browse url of buffer</h4>
<div class="outline-text-4" id="text-org832f321">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c i") #'browse-url-of-buffer)
</pre>
</div>
</div>
</div>

<div id="outline-container-orga732b0b" class="outline-4">
<h4 id="orga732b0b"><code>C-c j</code> Toggle window split</h4>
<div class="outline-text-4" id="text-orga732b0b">
<p>
<a href="https://www.emacswiki.org/emacs/ToggleWindowSplit">Toggling windows</a> from vertical to horizontal splits and vice-versa.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun toggle-window-split ()
  "Switch between horizontal and vertical split window layout."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (&lt;= (car this-win-edges)
                                         (car next-win-edges))
                                     (&lt;= (cadr this-win-edges)
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
</pre>
</div>
</div>
</div>

<div id="outline-container-org424b7e6" class="outline-4">
<h4 id="org424b7e6"><code>C-c k</code> kill all but one space</h4>
<div class="outline-text-4" id="text-org424b7e6">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c k") #'just-one-space)
</pre>
</div>
</div>
</div>

<div id="outline-container-org99a9b6d" class="outline-4">
<h4 id="org99a9b6d"><code>C-c q</code> replace regexp</h4>
<div class="outline-text-4" id="text-org99a9b6d">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c q") #'replace-regexp)
</pre>
</div>
</div>
</div>

<div id="outline-container-org344c10a" class="outline-4">
<h4 id="org344c10a"><code>C-c r</code> find recent files</h4>
<div class="outline-text-4" id="text-org344c10a">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c r") #'renz/find-recent-file)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgf92e6b1" class="outline-4">
<h4 id="orgf92e6b1"><code>C-c s</code> shell</h4>
<div class="outline-text-4" id="text-orgf92e6b1">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c s s") #'shell)
(global-set-key (kbd "C-c s e") #'eshell)
(global-set-key (kbd "C-c s t") #'term)
</pre>
</div>
</div>
</div>

<div id="outline-container-org9b6a9d7" class="outline-4">
<h4 id="org9b6a9d7"><code>C-c u</code> open URL at point in browser</h4>
<div class="outline-text-4" id="text-org9b6a9d7">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c u") #'browse-url-at-point)
</pre>
</div>
</div>
</div>

<div id="outline-container-org5a05e9c" class="outline-4">
<h4 id="org5a05e9c"><code>C-c v</code> faster git-commit</h4>
<div class="outline-text-4" id="text-org5a05e9c">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/git-commit ()
  (interactive)
  (vc-next-action nil)
  (log-edit-show-diff)
  (other-window 1))

(global-set-key (kbd "C-c v") #'renz/git-commit)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc449b10" class="outline-4">
<h4 id="orgc449b10"><code>C-c w</code> whitespace mode</h4>
<div class="outline-text-4" id="text-orgc449b10">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c w") #'whitespace-mode)
</pre>
</div>
</div>
</div>

<div id="outline-container-org200c857" class="outline-4">
<h4 id="org200c857"><code>C-c</code> Other bindings</h4>
<div class="outline-text-4" id="text-org200c857">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "C-c &lt;DEL&gt;") #'backward-kill-sexp)  ;; TTY-frindly
(global-set-key (kbd "C-c &lt;SPC&gt;") #'mark-sexp)  ;; TTY-friendly
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-orge46d83d" class="outline-3">
<h3 id="orge46d83d">F5-F9</h3>
<div class="outline-text-3" id="text-orge46d83d">
<p>
Like the <code>C-c &lt;letter&gt;</code> bindings, these are reserved for users.  In practice, even
though there are few of these keys, I tend to forget which is which.  So I wind
up using things bound to my <code>C-c</code> keymaps instead.  The <code>C-c</code> kyes from a more
natural, nested language in my head, so it feels more like I'm "speaking Emacs"
that way.
</p>
</div>
</div>

<div id="outline-container-orgeb927ff" class="outline-3">
<h3 id="orgeb927ff">Super bindings</h3>
<div class="outline-text-3" id="text-orgeb927ff">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(global-set-key (kbd "s-p") #'project-switch-project)
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-orgdf681ce" class="outline-2">
<h2 id="orgdf681ce">Text Completion</h2>
<div class="outline-text-2" id="text-orgdf681ce">
<p>
Emacs offers incredible depth and freedom when configuring methods to
automatically complete text.  There are actually two things that
"autocompletion" can refer to in Emacs:
</p>

<ol class="org-ol">
<li><a href="https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html">Minibuffer completion</a></li>
<li><a href="https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html">Completion at point</a></li>
</ol>

<p>
Emacs on its own does not have a nice pop-up-menu like Vim for completing text
at point.  For both the minibuffer and <code>completion-at-point</code> it uses a special
buffer called <code>*Completions*</code>, from which we can see (and optionally select) a
completion from potential candidates.  Before we get to tweak those settings,
though, we first need to oil the engine with an enhanced <i>completion style</i>
</p>
</div>

<div id="outline-container-org50d47fa" class="outline-3">
<h3 id="org50d47fa">Completion style</h3>
<div class="outline-text-3" id="text-org50d47fa">
<p>
For both the minibuffer and <code>completion-at-point</code>, I use the same <i>completion
style</i>.  Completion style is the method of assigning completion candidates to a
given input string.  <code>flex</code> is the built-in "fuzzy" completion style, familiar to
us from symbol completion in IDEs and VSCode's command palette.  <code>basic</code> functions
much like your default TAB-complete at a Bash shell.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq completion-styles '(flex basic partial-completion emacs22))
</pre>
</div>
</div>
</div>

<div id="outline-container-org3860151" class="outline-3">
<h3 id="org3860151">Nicer Display and Behavior of <code>*Completions*</code></h3>
<div class="outline-text-3" id="text-org3860151">
<p>
With the <i>completion style</i> set, we now have to configure the interface for
<i>displaying</i> candidates as we type.  First, I want candidates displayed as a
single, vertical list.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq completions-format 'one-column)
</pre>
</div>

<p>
Also, when using the built-in completion-at-point, the <code>*Completions*</code> buffer can
sometimes take up the whole screen when there are a lot of candidates.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(unless (version&lt; emacs-version "29.0")
  (setq completions-max-height 15))
</pre>
</div>

<p>
Some time ago, Prot wrote a package called <a href="https://github.com/protesilaos/mct/blob/main/mct.el">MCT</a> (Minibuffer and Completions in
Tandem) that enhanced the default minibuffer and <code>*Completions*</code> buffer behavior
to act more like what we expect of a modern editor's auto-complete.  He
discontinued development of that project once it became clear that Emacs 29 was
going to include similar behavior as a configurable option.  These are the
options in question.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(unless (version&lt; emacs-version "29.0")
  (setq completion-auto-help 'lazy
        completion-auto-select 'second-tab
        completion-show-help nil
        completions-sort nil
        completions-header-format nil))
</pre>
</div>
</div>
</div>

<div id="outline-container-org75d892f" class="outline-3">
<h3 id="org75d892f">Completion in the minibuffer and at point</h3>
<div class="outline-text-3" id="text-org75d892f">
<p>
By default, Emacs uses <code>M-TAB</code>, or the equivalent <code>C-M-i</code> for <code>completion-at-point</code>.
I'd much prefer to use the easier and more intuitive <code>TAB</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq tab-always-indent 'complete)
</pre>
</div>

<p>
Something I might try is to use <code>icomplete</code> along with <code>icomplete-in-buffer</code> to get
something like a little window that updates as I type.  It seems a little wonky,
since TAB-completion will still cause the &lowast;Completions&lowast; buffer to pop up, even
while Icomplete is active, unless we set <code>completion-auto-help</code> to <code>lazy</code>; and even
then it will still come up on the second TAB press.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq icomplete-in-buffer t)
(setq icomplete-prospects-height 10)
(icomplete-vertical-mode t)
</pre>
</div>

<p>
In the case that we need to enter a new file name, but <code>fido</code> is still showing a
completion candidate, you have to use <code>C-d</code> to refuse completion and take whatever
is currently in the prompt.  For instance, if we are editing a file <code>hello.py</code>,
and then use <code>C-x C-f hell.py</code>, the minibuffer will complete <code>hell.py</code> into <code>hello.py</code>
if we use <code>RET</code>, and will open a new buffer for <code>hell.py</code> if we use <code>C-d</code>.
</p>
</div>
</div>
</div>

<div id="outline-container-orgba42f05" class="outline-2">
<h2 id="orgba42f05">Language-specific major modes</h2>
<div class="outline-text-2" id="text-orgba42f05">
</div>
<div id="outline-container-org27f00d0" class="outline-3">
<h3 id="org27f00d0">Shell (Bash, sh, &#x2026;)</h3>
<div class="outline-text-3" id="text-org27f00d0">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/sh-indentation ()
  ;; (setq indent-tabs-mode t)
  (setq tab-width 8))

(add-hook 'sh-mode-hook #'renz/sh-indentation)
(add-hook 'bash-ts-mode-hook #'renz/sh-indentation)
</pre>
</div>
</div>
</div>
<div id="outline-container-org7ea582e" class="outline-3">
<h3 id="org7ea582e">HTML</h3>
<div class="outline-text-3" id="text-org7ea582e">
<p>
This changes the behavior of a few commonly-used tags in web pages that I write.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package sgml-mode
  :defer t
  :config
  (let* ((p-tag-old (assoc "p" html-tag-alist))
         ;; Close the &lt;p&gt; tag and open on a new line.
         (p-tag-new `("p" \n ,(cdr (cdr p-tag-old)))))
    (add-to-list 'html-tag-alist p-tag-new)
    ;; Close the &lt;code&gt; tag and stay inline.
    (add-to-list 'html-tag-alist '("code"))))

</pre>
</div>
</div>
</div>
<div id="outline-container-org0a991f0" class="outline-3">
<h3 id="org0a991f0">CSS</h3>
<div class="outline-text-3" id="text-org0a991f0">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq css-indent-offset 2)
</pre>
</div>

<p>
For validation, grab <a href="https://github.com/w3c/css-validator/releases/download/cssval-20220105/css-validator.jar">css-validator.jar</a> and execute it with java:
</p>

<pre class="example" id="org695e52d">
java -jar ~/.local/jars/css-validator.jar file:///home/me/my/site/index.html
</pre>
</div>
</div>

<div id="outline-container-org07bdbb7" class="outline-3">
<h3 id="org07bdbb7">Org-mode</h3>
<div class="outline-text-3" id="text-org07bdbb7">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq renz/org-home "~/.emacs.d/org/")
</pre>
</div>

<p>
<code>org-mode</code> provides <code>org-babel-tangle-jump-to-org</code>, which jumps back to an Org
source file from within the tangled code.  <code>renz/org-babel-tangle-jump-to-src</code>,
defined below, does the opposite - given the Org source file and point inside a
<code>src</code> block, it jumps to the location of the tangled code.  Provided by a helpful
<a href="https://emacs.stackexchange.com/a/69591">stackoverflow answer.</a>
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/org-babel-tangle-jump-to-src ()
  "The opposite of `org-babel-tangle-jump-to-org'.
Jumps to an Org src block from tangled code."
  (interactive)
  (if (org-in-block-p)
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
</pre>
</div>

<p>
Now we configure <code>org-mode</code> itself.  For a while I was trying <code>(setq
org-startup-indented t)</code> to get indentation under each header, but this was
interfering with the beautification features from <code>org-modern</code>.  Preferring the
latter over the former, I've removed the <code>org-startup-indented</code> call.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/list-files-with-absolute-path (directory)
  "Return a list of files in DIRECTORY with their absolute paths."
  (cl-remove-if-not #'file-regular-p (directory-files directory t ".*\.org$")))

(use-package org
  :hook
  ((org-mode . (lambda () (progn
                            (add-hook 'after-save-hook #'org-babel-tangle :append :local)
                            (add-hook 'org-babel-after-execute-hook #'renz/display-ansi-colors)
                            (setq indent-tabs-mode nil)))))

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
   ("C-c o k" . org-babel-remove-result)
   ("C-c o o" . renz/jump-org)
   ("C-c o y" . ox-clip-image-to-clipboard))

  :custom
  (org-image-actual-width nil "Enable resizing of images")
  (org-agenda-files (renz/list-files-with-absolute-path renz/org-home) "Sources for Org agenda view")
  (org-html-htmlize-output-type nil "See C-h f org-html-htmlize-output-type")
  (org-confirm-babel-evaluate nil "Don't ask for confirmation when executing src blocks")
  (org-goto-interface 'outline-path-completion "Use completing-read for org-goto (C-c C-j, nicer than imenu)")
  (org-outline-path-complete-in-steps nil "Flatten the outline path, instead of completing hierarchically")

  :config
  (add-to-list 'org-modules 'org-tempo)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (sql . t)
     (shell . t)
     (R . t)
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
     )))
</pre>
</div>
</div>

<div id="outline-container-orga1cde99" class="outline-4">
<h4 id="orga1cde99">Converting JSON to Org Tables</h4>
<div class="outline-text-4" id="text-orga1cde99">
<p>
I use a small external dependency for this:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package json-to-org-table
  :load-path "site-lisp/json-to-org-table/"
  :after org)
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-orgf0f0143" class="outline-3">
<h3 id="orgf0f0143">SQL</h3>
<div class="outline-text-3" id="text-orgf0f0143">
</div>
<div id="outline-container-orgd40602b" class="outline-4">
<h4 id="orgd40602b">DDL is SQL</h4>
<div class="outline-text-4" id="text-orgd40602b">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-to-list 'auto-mode-alist '("\\.ddl\\'" . sql-mode))
(add-to-list 'auto-mode-alist '("\\.bql\\'" . sql-mode))
</pre>
</div>
</div>
</div>

<div id="outline-container-org51d254d" class="outline-4">
<h4 id="org51d254d">Indentation</h4>
<div class="outline-text-4" id="text-org51d254d">
<p>
Vanilla Emacs doesn't offer a lot (read: nothing) in terms of making SQL code
pretty.  I tend to format SQL like this:
</p>

<div class="org-src-container">
<pre class="src src-sql">SELECT
    whatever,
    thing
FROM
    wherever AS w
    JOIN the_other AS t ON w.id = t.id
GROUP BY
    whatever
</pre>
</div>

<p>
The configuration of <code>sql-indent</code> below achieves that nicely when using <code>RET</code> and
<code>TAB</code> for formatting.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/sql-mode-hook ()
  (setq tab-width 4))

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
</pre>
</div>
</div>
</div>

<div id="outline-container-org35b94c3" class="outline-4">
<h4 id="org35b94c3">Interactive <code>hive2</code> mode</h4>
<div class="outline-text-4" id="text-org35b94c3">
<p>
This "hive2" package came from the days where I was working on an on-prem system
that used <code>hive2</code> as the main command-line interface to Hive.  I don't use this
much now, but it's a good reference for implementing a plug-in to a new
interactive SQL CLI.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package hive2
  :load-path "site-lisp/"
  :demand t
  :mode ("\\.hql" . sql-mode))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgfe17c05" class="outline-4">
<h4 id="orgfe17c05">Interactive <code>bq shell</code></h4>
<div class="outline-text-4" id="text-orgfe17c05">
<p>
The SQL interactive commands are looking for a single executable file, so let's
set that up somewhere common, like <code>~/.local/bin/bq-shell</code>.
</p>

<div class="org-src-container">
<pre class="src src-shell">#!/usr/bin/env sh
bq shell "$@"
</pre>
</div>

<p>
Also, we don't want to use "legacy SQL" in our queries, which requires us to
configure the <code>bq query</code> statically in a <code>~/.bigqueryrc</code> file, according to the
Google <a href="https://issuetracker.google.com/issues/35905841">issue tracker</a>.
</p>

<div class="org-src-container">
<pre class="src src-:tangle">[query]
--use_legacy_sql=false
</pre>
</div>

<p>
Then enable the BQ product.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package bq
  :load-path "site-lisp"
  :demand t)
</pre>
</div>
</div>
</div>

<div id="outline-container-orgb64a098" class="outline-4">
<h4 id="orgb64a098">BigQuery <code>sql</code> Blocks in Org-Babel</h4>
<div class="outline-text-4" id="text-orgb64a098">
<p>
Advising <code>org-babel-execute:sql</code> in this way allows me to use <code>#+begin_src sql
:engine bq :results raw</code> blocks in org-babel and execute them with <code>C-c C-c</code>.  More
commonly, though, I set <code>#+PROPERTY: header-args:sql :engine bq :results raw</code> at
the top of the document so that I can just mark a <code>src</code> block as <code>sql</code> and be done
with it.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun org-babel-execute:bq (orig-fun body params)
  (if (string-equal-ignore-case (cdr (assq :engine params)) "bq")
      (json-to-org-table-parse-json-string
       (org-babel-execute:shell (concat "bq query --format=json --nouse_legacy_sql '" body "'")
                                params))
    (org-babel-execute:sql body params)))

(advice-add 'org-babel-execute:sql :around #'org-babel-execute:bq)
</pre>
</div>

<p>
This also typically requires <code>#+OPTIONS: ^:nil</code> at the top of the Org document to
stop underscores from messing up how column names are displayed.
</p>
</div>
</div>

<div id="outline-container-orga20fed7" class="outline-4">
<h4 id="orga20fed7"><span class="todo TODO">TODO</span> BigQuery exception markers</h4>
<div class="outline-text-4" id="text-orga20fed7">
<p>
When running BigQuery from a <code>*compilation*</code> buffer, it would be nice if I could get
error markers to jump directly to the issue.
</p>
</div>
</div>
</div>

<div id="outline-container-org637c83c" class="outline-3">
<h3 id="org637c83c">Python</h3>
<div class="outline-text-3" id="text-org637c83c">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-to-list 'auto-mode-alist '("Pipfile" . toml-ts-mode))
</pre>
</div>
</div>
<div id="outline-container-orgb305a40" class="outline-4">
<h4 id="orgb305a40">Pyright error links in <code>*compilation*</code></h4>
<div class="outline-text-4" id="text-orgb305a40">
<p>
The <code>M-x compile</code> feature does not recognize or parse <code>pyright</code> error messages out
of the box, so I add that support myself.  Here's an example error message:
</p>

<pre class="example" id="orgbd3dbe7">
/home/robb/tmp/errors.py/
  /home/robb/tmp/errors.py:1:1 - error: "foo" is not defined (reportUndefinedVariable)
  /home/robb/tmp/errors.py:1:1 - warning: Expression value is unused (reportUnusedExpression)
  /home/robb/tmp/errors.py:4:12 - error: Operator "+" not supported for types "str" and "Literal[1]"
    Operator "+" not supported for types "str" and "Literal[1]" (reportGeneralTypeIssues)
2 errors, 1 warning, 0 informations
</pre>

<p>
To get the basic <code>M-g M-n</code> and <code>M-g M-p</code> navigation working, we just need a regex to
parse file name, line, and column number.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(pyright "^[[:blank:]]+\\(.+\\):\\([0-9]+\\):\\([0-9]+\\).*$" 1 2 3))
  (add-to-list 'compilation-error-regexp-alist 'pyright))
</pre>
</div>

<p>
It would be nice if we could also capture the <code>\\(error\\|warning\\)</code> part as
"KIND", but I'm struggling to get it working.
</p>
</div>
</div>

<div id="outline-container-orga163f82" class="outline-4">
<h4 id="orga163f82">Python check with "ruff"</h4>
<div class="outline-text-4" id="text-orga163f82">
<p>
Another nice vanilla feature of <code>python-mode</code> is <code>M-x python-check</code>, which runs a
pre-specified linter.  Setting that to <code>mypy</code> or <code>pyright</code> if either of those
programs exist is a small time saver.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package python
  :config
  (require 'eglot)
  (setq python-check-command "ruff")
  (add-hook 'python-mode-hook #'flymake-mode)
  (add-hook 'python-ts-mode-hook #'flymake-mode)
  ;; (add-to-list 'eglot-server-programs '((python-mode python-ts-mode) "ruff-lsp"))
  )
</pre>
</div>
</div>
</div>


<div id="outline-container-org537a13b" class="outline-4">
<h4 id="org537a13b">Fix Microsoft Windows Issues</h4>
<div class="outline-text-4" id="text-org537a13b">
<p>
At one point, I ran into something similar to this <a href="https://github.com/jorgenschaefer/elpy/issues/733">elpy issue</a> on Windows.  The
culprit was "App Execution Aliases" with python and python3 redirecting to the
windows store.  Using this fixed it:
</p>

<pre class="example" id="orga455a62">
winkey -&gt; Manage app execution aliases -&gt; uncheck python and python3
</pre>

<p>
Also on Windows - a <code>pip install</code> of <code>pyreadline3</code> is required to make
tab-completion work at all. It provides the <code>readline</code> import symbol.
</p>
</div>
</div>

<div id="outline-container-orgae7ebf3" class="outline-4">
<h4 id="orgae7ebf3">Make check command and virtualenv root safe for .dir-locals.el</h4>
<div class="outline-text-4" id="text-orgae7ebf3">
<p>
Virtualenvs require <code>.dir-locals.el</code> to have something like:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">((python-mode . ((python-shell-virtualenv-root . "/path/to/my/.venv"))))
</pre>
</div>

<p>
However, this only operates on `run-python' shells.  Also, for projects, we need to
make sure that setting the virtualenv root is marked as safe.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(put 'python-check-command 'safe-local-variable #'stringp)
(put 'python-shell-virtualenv-root 'safe-local-variable #'stringp)
(put 'pyvenv-default-virtual-env-name 'safe-local-variable #'stringp)
</pre>
</div>
</div>
</div>

<div id="outline-container-org394c26c" class="outline-4">
<h4 id="org394c26c">Emacs Jupyter?</h4>
<div class="outline-text-4" id="text-org394c26c">
<p>
Eventually, I would like to try the <a href="https://github.com/dzop/emacs-jupyter">emacs-jupyter</a> package to interface with
Jupyter kernels from org-mode.
</p>
</div>
</div>

<div id="outline-container-orgba95109" class="outline-4">
<h4 id="orgba95109">pyrightconfig.json</h4>
<div class="outline-text-4" id="text-orgba95109">
<p>
The most consistent way to get <code>eglot</code> to properly configure the python virtual
environment with <code>pyright</code> is to have a static file at the root of the project,
called <code>pyrightconfig.json</code>.  I wrote a short plugin that allows me to select a
directory using <code>completing-read</code> and have Emacs write the content of
<code>pyrightconfig.json</code> based on what I selected, in the appropriate directory.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package pyrightconfig
  :after (python))
</pre>
</div>

<p>
Configuring pyright this way rather than "activating" an environment through
Emacs (ala <code>pythonic-activate</code> or similar) means we can be running the language
server in more than one project at a time, each pointing to its respective
virtual environment.
</p>
</div>
</div>

<div id="outline-container-org918a5f9" class="outline-4">
<h4 id="org918a5f9">Activating Virtual Environments Over Tramp</h4>
<div class="outline-text-4" id="text-org918a5f9">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package tramp-venv
  :bind
  (("C-c t v a" . tramp-venv-activate)
   ("C-c t v d" . tramp-venv-deactivate)))
</pre>
</div>
</div>
</div>

<div id="outline-container-org3e78f1f" class="outline-4">
<h4 id="org3e78f1f">Pyvenv for virtual environments</h4>
<div class="outline-text-4" id="text-org3e78f1f">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package pyvenv
  :init
  (if (eq system-type 'darwin)
      (setenv "WORKON_HOME" "~/micromamba/envs/")
    (setenv "WORKON_HOME" "~/.conda/envs/"))
  :bind
  (("C-c p w" . pyvenv-workon)
   ("C-c p d" . pyvenv-deactivate)
   ("C-c p a" . pyvenv-activate))
  :config
  (pyvenv-mode))
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org9c37b81" class="outline-3">
<h3 id="org9c37b81">Markdown</h3>
<div class="outline-text-3" id="text-org9c37b81">
<p>
When installing <code>markdown</code> through Anaconda, the executable is actually called
<code>markdown_py</code>.  In case <code>markdown</code> isn't found, use that instead.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(when (and (not (executable-find "markdown")) (executable-find "markdown_py"))
  (setq markdown-command "markdown_py"))
</pre>
</div>

<p>
Some folks like to write markdown without hard line breaks.  When viewing those
documents, I can use <code>M-x renz/md-hook</code> to view it as if there were line breaks in
it.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/md-hook ()
  "View buffer in visual fill mode with 80 character width."
  (interactive)
  (visual-fill-column-mode)
  (setq-local fill-column 80))
</pre>
</div>

<p>
I make a lot of spelling mistakes as I type&#x2026;
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'markdown-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)
</pre>
</div>

<p>
And I like to see language syntax highlighting within code fences.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq markdown-fontify-code-blocks-natively t)
</pre>
</div>
</div>
</div>

<div id="outline-container-org4e27af8" class="outline-3">
<h3 id="org4e27af8">Missing auto-modes</h3>
<div class="outline-text-3" id="text-org4e27af8">
<p>
These really should already be in <code>auto-mode-alist</code>, but aren't for some reason.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.dockerfile\\'" . dockerfile-ts-mode))
</pre>
</div>
</div>
</div>

<div id="outline-container-orge210326" class="outline-3">
<h3 id="orge210326">csv-mode</h3>
<div class="outline-text-3" id="text-orge210326">
<p>
Handy for viewing data quickly.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package csv-mode
  :mode "\\.csv\\'")
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-orgf8e4410" class="outline-2">
<h2 id="orgf8e4410">Tool configuration</h2>
<div class="outline-text-2" id="text-orgf8e4410">
<p>
These are tweaks for self-contained tooling, such as third party packages or
built-in packages that have a well-defined scope and namespace.
</p>
</div>

<div id="outline-container-org06f8838" class="outline-3">
<h3 id="org06f8838"><code>eldoc</code></h3>
<div class="outline-text-3" id="text-org06f8838">
<p>
I find it very distracting when <code>eldoc</code> suddenly pops up and consumes a large part
of the screen for docstrings in python.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq eldoc-echo-area-use-multiline-p nil)
</pre>
</div>
</div>
</div>

<div id="outline-container-org0a4d0d3" class="outline-3">
<h3 id="org0a4d0d3"><code>imenu</code></h3>
<div class="outline-text-3" id="text-org0a4d0d3">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package imenu
  :config
  (setq imenu-auto-rescan t
        org-imenu-depth 3))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgceac95c" class="outline-3">
<h3 id="orgceac95c"><code>dabbrev</code></h3>
<div class="outline-text-3" id="text-orgceac95c">
<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package dabbrev
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))
</pre>
</div>
</div>
</div>

<div id="outline-container-org0cc59ee" class="outline-3">
<h3 id="org0cc59ee"><code>dired</code></h3>
<div class="outline-text-3" id="text-org0cc59ee">
<p>
By default, <code>dired</code> uses bytes instead of "K", "Mb", or "G" for file sizes.  I
also have it hide the mode, size, and owner of each file by default.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package dired
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq dired-listing-switches "-alFh")
  (setq dired-dwim-target t))
</pre>
</div>

<p>
Also enabled above is Do-What-I-Mean (DWIM) copying.  This is for when two dired
windows are open, and we want to copy something from one location to the other.
By enabling <code>dired-dwim-target</code>, it auto-populates the minibuffer with the other
dired window's path when issuing a copy command with <code>C</code>.
</p>
</div>
</div>

<div id="outline-container-org3ee799b" class="outline-3">
<h3 id="org3ee799b">Visual fill column</h3>
<div class="outline-text-3" id="text-org3ee799b">
<p>
For visual lines, this adds line breaks at the fill-column value.  Especially
useful for prose that is meant to be copied to other mediums, such as email or
word.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package visual-fill-column
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode))
</pre>
</div>
</div>
</div>

<div id="outline-container-org275bae8" class="outline-3">
<h3 id="org275bae8">eww - search engine and browser</h3>
<div class="outline-text-3" id="text-org275bae8">
<p>
Ecosia requires JavaScript, unfortunately.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package eww
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))
</pre>
</div>
</div>
</div>

<div id="outline-container-orgc7a1a4f" class="outline-3">
<h3 id="orgc7a1a4f">Reloading Emacs</h3>
<div class="outline-text-3" id="text-orgc7a1a4f">
<p>
Often used when changing up my <code>init.el</code>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package restart-emacs
  :bind ("C-c x r" . restart-emacs))
</pre>
</div>
</div>
</div>

<div id="outline-container-org0ece816" class="outline-3">
<h3 id="org0ece816">Language Server Protocol (LSP) with <code>eglot</code></h3>
<div class="outline-text-3" id="text-org0ece816">
<p>
As of version 29, <a href="https://github.com/joaotavora/eglot">eglot</a> (Emacs polyGLOT) is bundled with Emacs.  It provides Emacs with the
client side configuration for the <a href="https://microsoft.github.io/language-server-protocol/">language server protocol</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package eglot
  :bind (("C-c l c" . eglot-reconnect)
         ("C-c l d" . flymake-show-buffer-diagnostics)
         ("C-c l f f" . eglot-format)
         ("C-c l f b" . eglot-format-buffer)
         ("C-c l l" . eglot)
         ("C-c l r n" . eglot-rename)
         ("C-c l s" . eglot-shutdown)))
</pre>
</div>

<p>
To have <code>eglot</code> always start up for a python buffer, we would tangle this line
into <code>init.el</code>.  However, this can cause a significant loading delay over Tramp,
and I would prefer snappy, simple access with LSP provided on an as-needed
basis.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'python-mode-hook 'eglot-ensure)
</pre>
</div>
</div>

<div id="outline-container-org9e3cf88" class="outline-4">
<h4 id="org9e3cf88">Side show: <code>semantic-mode</code></h4>
<div class="outline-text-4" id="text-org9e3cf88">
<p>
For a while, it looks like Emacs was trying out something called <a href="https://www.gnu.org/software/emacs/manual/html_node/semantic/Semantic-mode.html">semantic-mode</a>,
which looks a lot like a precursor to what we now know as the <a href="https://microsoft.github.io/language-server-protocol/">Language Server
Protocol</a>.  Enabling it was done through adding the <code>semantic-mode</code> hook to your
language's major mode hook:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(add-hook 'python-mode-hook 'semantic-mode)
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-org81f7321" class="outline-3">
<h3 id="org81f7321">TreeSitter</h3>
<div class="outline-text-3" id="text-org81f7321">
</div>
<div id="outline-container-org4c8a9ce" class="outline-4">
<h4 id="org4c8a9ce">About TreeSitter and its Load Paths</h4>
<div class="outline-text-4" id="text-org4c8a9ce">
<p>
Emacs 29 added native <a href="https://tree-sitter.github.io/tree-sitter/">TreeSitter</a> support.  TreeSitter is a new way of
incrementally parsing source code that offers superior navigation and syntax
highlighting.  To fully realize this benefit, however, it requires that we
install <code>tree-sitter</code> grammars independently from Emacs.  Right now, I'm using
<a href="https://github.com/casouri/tree-sitter-module">casouri's modules</a>, which I build and install under <code>~/.emacs.d/tree-sitter</code>, if
they don't already exist under <code>/usr/local/lib/</code> or <code>~/.local/lib</code>.  In case of the
latter, I just add extra paths to <code>treesit-extra-load-path</code> explicitly.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(when (boundp 'treesit-extra-load-path)
  (add-to-list 'treesit-extra-load-path "/usr/local/lib/")
  (add-to-list 'treesit-extra-load-path "~/.local/lib/"))
</pre>
</div>

<p>
For the full instructions, the commit history of adding the <code>tree-sitter</code> modules
to Emacs included a <a href="https://git.savannah.gnu.org/cgit/emacs.git/plain/admin/notes/tree-sitter/starter-guide?h=feature/tree-sitter">full guide</a>, which can be read in Info under "Parsing Program
Source".
</p>

<pre class="example" id="org3e520bc">
C-h i d m elisp RET g Parsing Program Source RET
</pre>

<p>
Enabling TreeSitter is done on a per-language basis to override the default
major mode with the corresponding TreeSitter version.
</p>
</div>
</div>

<div id="outline-container-org4bef761" class="outline-4">
<h4 id="org4bef761">Automatically Using TreeSitter Modes</h4>
<div class="outline-text-4" id="text-org4bef761">
<p>
I've posted this to GitHub and MELPA as <a href="https://github.com/renzmann/treesit-auto">treesit-auto</a>.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode))
</pre>
</div>

<p>
Before it was published to MELPA, I used a git subtree to manage the plugin.
This is a pretty useful technique, so I keep these two one-liners around in case
I need to reference or copy them.  To get a copy of something as a subtree, I
use this:
</p>

<div class="org-src-container">
<pre class="src src-shell">git subtree add -P site-lisp/treesit-auto git@github.com:renzmann/treesit-auto main --squash
</pre>
</div>

<p>
Fetching updates is a similar command.
</p>

<div class="org-src-container">
<pre class="src src-shell">git subtree pull -P site-lisp/treesit-auto git@github.com:renzmann/treesit-auto main --squash
</pre>
</div>
</div>
</div>

<div id="outline-container-orgcc7c200" class="outline-4">
<h4 id="orgcc7c200">Ooo, aaah, shiny colors</h4>
<div class="outline-text-4" id="text-orgcc7c200">
<p>
I like to program "in Skittles":
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq-default treesit-font-lock-level 3)
</pre>
</div>
</div>
</div>
</div>

<div id="outline-container-orgc357dc4" class="outline-3">
<h3 id="orgc357dc4">Tramp</h3>
<div class="outline-text-3" id="text-orgc357dc4">
<p>
Tramp (Transparent Remote Access Multiple Protocol) allows us to access files on
a remote machine, and edit them locally.  This is great for simple changes or
quickly testing out some Python on a VM somewhere.  It isn't as snappy as using
the TTY version or an X-forwarded Emacs from the server directly, so if I <i>can</i>
set up Emacs remotely, I usually do.  When I don't want to or don't have the
time, Tramp is a godsend.  There are, however, many foibles to guard against,
particularly with how interacts with version control and <code>.dir-locals</code>.  The
Tramp manual (distributed with Emacs) recommends adjusting these for some speed
improvements:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(use-package tramp
  :defer t
  :config
  (setq vc-handled-backends '(Git)
        file-name-inhibit-locks t
        tramp-inline-compress-start-size 1000
        tramp-copy-size-limit 10000
        tramp-verbose 1)
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))
</pre>
</div>

<p>
eglot is <a href="https://github.com/joaotavora/eglot/issues/859">actively working</a> on an issue related to timers causing a "Forbidden
reentrant call of Tramp" message and freezing.  In the meantime, this setting
was recommended.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq tramp-use-ssh-controlmaster-options nil)
</pre>
</div>

<p>
For some time I was having a lot of trouble with prohibitive slowness over
Tramp, and after careful scrutiny of the logs on (I believe) <code>tramp-verbose 6</code>, I
found out that enabling remote dir-locals was causing a huge bottleneck.  On
every operation it would trace up the filesystem tree back to the root
directory, scanning for a <code>.dir-locals</code> file.  Since some of the drives were
network-mounted, this caused thousands of network calls per file operation,
obviously slowing things down a lot.  Because of this, I've opted to simply
disable <code>.dir-locals</code> over Tramp entirely, since I don't really use it much, if at
all.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">;; (setq enable-remote-dir-locals t)
</pre>
</div>

<p>
<a href="https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html">Disabling VC</a> <i>does</i> seem to speed things up a little, but it's not an acceptable
thing to put in, since I so frequently use VC over tramp.  Fully disabling VC
would include this snippet:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(remove-hook 'find-file-hook 'vc-find-file-hook)

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))
</pre>
</div>

<p>
Additionally, these came up as other potential options <a href="https://github.com/doomemacs/doomemacs/issues/3909">from the doom-emacs
issues</a>, which I do not currently include.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(setq tramp-default-method "scp")
(setq projectile--mode-line "Projectile")
</pre>
</div>

<p>
I often need to set these in ~/.ssh/config for TRAMP to speed up
</p>

<pre class="example" id="org73b42aa">
Host *
     ControlMaster auto
     ControlPath ~/.ssh/master-%h:%p
     ControlPersist 10m
     ForwardAgent yes
     ServerAliveInterval 60
</pre>
</div>
</div>

<div id="outline-container-org21639ae" class="outline-3">
<h3 id="org21639ae">Shell commands</h3>
<div class="outline-text-3" id="text-org21639ae">
<p>
The Async command buffer's default behavior is to print <code>^M</code> characters (the
carriage return) instead of actually clearing text.  This is problematic for
spinners and progress bars, so I have a little hack to work around that.
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(defun renz/async-shell-command-filter-hook ()
  "Filter async shell command output via `comint-output-filter'."
  (when (equal (buffer-name (current-buffer)) "*Async Shell Command*")
    ;; When `comint-output-filter' is non-nil, the carriage return characters ^M
    ;; are displayed
    (setq-local comint-inhibit-carriage-motion nil)
    (when-let ((proc (get-buffer-process (current-buffer))))
      ;; Attempting a solution found here:
      ;; https://gnu.emacs.help.narkive.com/2PEYGWfM/m-chars-in-async-command-output
      (set-process-filter proc 'comint-output-filter))))


(add-hook 'shell-mode-hook #'renz/async-shell-command-filter-hook)
</pre>
</div>

<p>
There might be a better way, but this mostly works for now.
</p>
</div>
</div>
</div>

<div id="outline-container-org95eef66" class="outline-2">
<h2 id="org95eef66">Footer</h2>
<div class="outline-text-2" id="text-org95eef66">
<p>
Thank you for reading 'till the end or for being interested on how to end an
Emacs package.  So that's it, let's gracefully finish tangling everything:
</p>

<div class="org-src-container">
<pre class="src src-emacs-lisp">(provide 'init.el)
;;; init.el ends here
</pre>
</div>
</div>
</div>
</div>
<div id="postamble" class="status">
<p class="author">Author: Robb Enzmann</p>
<p class="date">Created: 2023-10-26 Thu 16:48</p>
<p class="validation"><a href="https://validator.w3.org/check?uri=referer">Validate</a></p>
</div>
</body>
</html>
