;;; private/org/+trello.el -*- lexical-binding: t; -*-

(def-package! org-trello
  :config
  ;; org-trello major mode for all .org files
  ;; (add-hook 'org-mode-hook 'org-trello-mode)
  ;; org-trello major mode for all .trello files
  (add-to-list 'auto-mode-alist '("\\.trello$" . org-mode))
  ;; add a hook function to check if this is trello file, then activate the org-trello minor mode.
  (add-hook 'org-mode-hook
            (lambda ()
              (let ((filename (buffer-file-name (current-buffer))))
                (when (and filename (string= "trello" (file-name-extension filename)))
                (org-trello-mode))))))
