;; The journey here...
;; 1. tried python-shell-exec-path and python-shell-interpreter, but those are only for the REPL
;; 2. tried setting exec-path, but that's only for specific commands, like =M-x grep=
;; 3. had to read up on the alist and .dir-locals.el examples/syntax
;; 4. exec-path and PATH aren't necessarily in sync
((nil . ((eval . (setenv "PATH" (concat "/home/robb/repos/renzmann.github.io/content/posts/006_emacs_2_python/.venv/bin" ":" (getenv "PATH"))))
	 (python-check-command . "mypy"))))
