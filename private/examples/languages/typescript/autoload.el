;;; private/typescript/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
 (defun my-setup-tide-mode ()
  "Enable `tide-mode'."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;;;###autoload
 (defun my-typescript-init-tide ()
  (when (or (eq major-mode 'typescript-mode)
            (and (eq major-mode 'web-mode)
                 buffer-file-name
                 (equal (file-name-extension buffer-file-name) "ts")))
    (tide-setup)
    (flycheck-mode +1)
    (eldoc-mode +1)
    (setq tide-project-root (doom-project-root))))

