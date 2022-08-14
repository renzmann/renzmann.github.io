;; The journey here...
;; 1. tried python-shell-exec-path and python-shell-interpreter, but those are only for the REPL
;; 2. tried setting exec-path, but that's only for specific commands, like =M-x grep=
;; 3. had to read up on the alist and .dir-locals.el examples/syntax
;; 4. exec-path and PATH aren't necessarily in sync
;; 5. "mypy" and "pyright" aren't one of the default python-check-commmand programs

;; This STILL doesn't work, because it won't be buffer-local. i.e. switching back to init.el will still have this venv at the front of PATH
;; ((nil . ((eval . (setenv "PATH" (concat "/home/robb/repos/renzmann.github.io/content/posts/006_emacs_2_python/.venv/bin" ":" (getenv "PATH"))))
;; 	 (python-check-command . "mypy"))))
((python-mode . ((python-check-command . "mypy"))))
