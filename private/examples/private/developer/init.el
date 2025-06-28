;;; private/tools/config.el -*- lexical-binding: t; -*-


;; (def-package-hook! emacs-snippets :disable t)

;; If a :pre-init / :pre-config hook returns nil, it overwrites that package's
;; original :init / :config block. Exploit this to overwrite Doom's config.
(def-package-hook! doom-themes
  :pre-config
  (setq doom-neotree-file-icons t)
  nil)

;; ...otherwise, make sure they always return non-nil!
(def-package-hook! evil
  :pre-init
  (setq evil-magic nil)
  t)