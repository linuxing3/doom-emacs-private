;;; feature-x/spellcheck/config.el -*- lexical-binding: t; -*-


(def-package! flyspell ; built-in
  :defer t
  :init
  :config
  (setq ispell-program-name (executable-find "aspell")
        ispell-list-command "--list"
        ispell-extr-args '("--dont-tex-check-comments"))

  (defun +spellcheck|immediately ()
    "Spellcheck the buffer when `flyspell-mode' is enabled."
    (when (and flyspell-mode +spellcheck-immediately)
      (flyspell-buffer))))
