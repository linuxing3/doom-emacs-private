;;; ~/.config/doom.d/extensions/org+oram.el -*- lexical-binding: t; -*-

(use-package! org-roam-mode
  :defer t
  :hook (org-mode . org-roam-mode)
  :when (featurep! :lang org)
  :init
  ;; variables
  (setq org-roam-directory (dropbox-path "org/roam/"))
  (setq org-roam-filename-noconfirm nil)
  (setq org-roam-filename-noconfirm nil)
  (setq org-roam-graphviz-executable "dot")
  (setq org-roam-graph-viewer "microsoft edge"))
