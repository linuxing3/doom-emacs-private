;;; lang-x/javascript/+term.el -*- lexical-binding: t; -*-

;; tern :- IDE like features for javascript and completion
;; http://ternjs.net/doc/manual.html#emacs
(def-package! tern
  :config
  (defun my-js-mode-hook ()
    "Hook for `js-mode'."
    (set-company-backends! 'js2-mode 'company-tern 'company-files)
  (add-hook 'js2-mode-hook 'my-js-mode-hook)
  (add-hook 'js2-mode-hook 'company-mode))
(add-hook 'js2-mode-hook 'tern-mode)

;; company backend for tern
;; http://ternjs.net/doc/manual.html#emacs
(def-package! company-tern)
