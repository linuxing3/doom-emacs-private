;;; c:/Users/Administrator/.doom.d/extensions/org+oram.el -*- lexical-binding: t; -*-

(use-package! org-roam-mode
  :defer t
  :hook (org-mode . org-roam-mode)
  :when (featurep! :lang org)
  :init
  ;; variables
  (setq org-roam-directory "D:/Dropbox/org/roam/")
  (setq org-roam-filename-noconfirm nil)
  (setq org-roam-filename-noconfirm nil)
  (setq org-roam-graphviz-executable "dot.exe")
  (setq org-roam-graph-viewer "chrome.exe")
  (setq org-roam-templates
        (list (list "default" (list :file #'org-roam--file-name-timestamp-title
                                    :content "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}"))
              (list "private" (list :file #'linuxing3/org-roam-title-private
                                    :content "#+DATE: write your date
#+AUTHOR: linuxing
#+EXCERPT: emacs
#+TITLE: ${title}"))))
  (defun linuxing3/org-roam-title-private (title)
    (let ((timestamp (format-time-string "%Y-%m-%d" (current-time)))
          (slug (org-roam--title-to-slug title)))
      (format "%s-%s" timestamp slug)))
  :bind (:map org-roam-mode-map
          (("C-c n l" . org-roam)
           ("C-c n f" . org-roam-find-file)
           ("C-c n g" . org-roam-show-graph))
          :map org-mode-map
          (("C-c n i" . org-roam-insert))))
