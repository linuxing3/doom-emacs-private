#!/usr/bin/env sh
":"; exec emacs --quick --script "$0" -- "$@" # -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq log-file (format "%s-log.txt" (file-name-base load-file-name)))

(load (expand-file-name "initialise.el" (file-name-directory load-file-name)) nil t)
(initialise t)

;;; Actually do the exporting now

(message "[34] Opening config file: %s"
         (expand-file-name "config.org" config-root))

(require 'vc) ; need this for modification-time macro
(require 'org)
(require 'ox-latex)

(setq org-mode-hook nil)
(with-temp-buffer
  (let ((default-directory config-root)
        (buffer-file-name (expand-file-name "config.org" config-root))
        (org-export-coding-system org-html-coding-system)
        org-mode-hook org-load-hook)
    (insert-file-contents (expand-file-name "config.org" config-root))
    (goto-char (point-max))
    (insert (shell-command-to-string (expand-file-name "./gen-org-snippets.sh" script-root)))
    (message "[33] Exporting %s" (buffer-file-name))
    (org-latex-export-to-pdf)))

(publish "config.pdf")

(message "[1;32] Config export complete!")

(setq inhibit-message t)
(kill-emacs exit-code)
