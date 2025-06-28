(def-package! evil
  :init
  (setq
        ;; cursor appearance
        evil-default-cursor 'my-evil-default-cursor
        evil-normal-state-cursor 'box
        evil-emacs-state-cursor  '(box my-evil-emacs-cursor)
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :config
  ;; (add-hook 'doom-init-hook #'evil-mode)
  ;; Change the cursor color in emacs mode
  (defvar my-evil--default-cursor-color "#eeefff")
  (defun my-evil-default-cursor () (set-cursor-color my-evil--default-cursor-color))
  (defun my-evil-emacs-cursor ()   (set-cursor-color (face-foreground 'warning)))
  )


(def-package! evil-escape
  :commands (evil-escape evil-escape-mode evil-escape-pre-command-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.25)
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  (map! :irvo "C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp))

(def-package! evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))
