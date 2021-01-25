;;; c:/Users/Administrator/.doom.d/extensions/org+oram.el -*- lexical-binding: t; -*-

(use-package! org-roam-mode
  :defer t
  :init
  ;; variables
  (setq org-roam-directory (dropbox-path "org/roam/"))
  (setq org-roam-graphviz-executable "dot.exe")
  (setq org-roam-graph-viewer "chrome.exe"))
