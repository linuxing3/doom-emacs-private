;;; c:/Users/Administrator/.doom.d/extensions/org+oram.el -*- lexical-binding: t; -*-

(use-package! org-roam-mode
  :defer t
  :init
  ;; variables
  (setq org-roam-directory (dropbox-path "org/roam/"))
  (setq org-roam-graphviz-executable "dot.exe")
  (setq org-roam-graph-viewer "chrome.exe"))

(use-package org-roam-server
  :after (org-roam server)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))
