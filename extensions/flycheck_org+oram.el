;;; c:/Users/Administrator/.doom.d/extensions/org+oram.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :hook
  (after-init . org-roam-mode)
  :config
  (progn
    (defun linuxing3/org-roam-title-private (title)
      (let ((timestamp (format-time-string "%Y%m%d%H%M%S" (current-time)))
            (slug (org-roam--title-to-slug title)))
        (format "private-%s_%s" timestamp slug)))
    (org-roam-directory "D:/Dropbox/org/roam/")
    (setq org-roam-filename-noconfirm nil)
    (setq org-roam-graphviz-executable "c:/program files (x86)/graphviz2.38/bin/dot.exe")
    (setq org-roam-graph-viewer "C:/Users/Administrator/AppData/Local/Google Chrome Application/chrome.exe")
    (setq org-roam-templates
          (list (list "default" (list :file #'org-roam--file-name-timestamp-title
                                      :content "#+SETUPFILE:./hugo_setup.org
#+HUGO_SECTION: zettels
#+HUGO_SLUG: ${slug}
#+TITLE: ${title}"))
                (list "private" (list :file #'linuxing3/org-roam-title-private
                                      :content "#+TITLE: ${title}"))))
    )
  :bind (:map org-roam-mode-map
          (("C-c n l" . org-roam)
           ("C-c n f" . org-roam-find-file)
           ("C-c n g" . org-roam-show-graph))
          :map org-mode-map
          (("C-c n i" . org-roam-insert))))
