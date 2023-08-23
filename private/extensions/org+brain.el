;;; private/org/+brain.el -*- lexical-binding: t; -*-

(use-package! org-brain
  :init
  (setq org-brain-path "D:/Dropbox/org/brain")
  ;; For Evil users
  (after! evil
   (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (defun org-brain-deft ()
    "Use `deft' for files in `org-brain-path'."
    (interactive)
    (let ((deft-directory org-brain-path)
          (deft-recursive t)
          (deft-extensions '("org")))
      (deft)))
  (defun org-brain-insert-resource-icon (link)
    "Insert an icon, based on content of org-mode LINK."
    (insert (format "%s "
                    (cond ((string-prefix-p "http" link)
                          (cond ((string-match "wikipedia\\.org" link)
                                  (all-the-icons-faicon "wikipedia-w"))
                                ((string-match "github\\.com" link)
                                  (all-the-icons-octicon "mark-github"))
                                ((string-match "vimeo\\.com" link)
                                  (all-the-icons-faicon "vimeo"))
                                ((string-match "youtube\\.com" link)
                                  (all-the-icons-faicon "youtube"))
                                (t
                                  (all-the-icons-faicon "globe"))))
                          ((string-prefix-p "brain:" link)
                          (all-the-icons-fileicon "brain"))
                          (t
                          (all-the-icons-icon-for-file link))))))
  (add-hook 'org-brain-after-resource-button-functions #'org-brain-insert-resource-icon)
  ;; tracking
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  ;; capture
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  ;; ui
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))
