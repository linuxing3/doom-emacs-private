;;; private/extensions/exwm.el -*- lexical-binding: t; -*-
;;; exwm.el -*- lexical-binding: t; -*-

;; i3-like keybindings for EXWM
(windmove-mode +1)
(when (featurep! :private exwm)
  (map! "s-j" #'windmove-left
        "s-k" #'windmove-down
        "s-l" #'windmove-up
        "s-;" #'windmove-right
        "s-J" #'windmove-swap-states-left
        "s-K" #'windmove-swap-states-down
        "s-L" #'windmove-swap-states-up
        "s-Q" #'delete-window
        "s-:" #'windmove-swap-states-right
        "s-v" #'split-window-right
        "s-h" #'split-window-below
        (:when (featurep! :ui workspaces)
          (:when (featurep! :term vterm)
            "s-<return>" (defun +run-or-raise-vterm ()
                           (interactive)
                           (+workspace-switch "Vterm" t)
                           (let ((display-buffer-alist))
                             (vterm most-positive-fixnum)))))
        "s-d" #'app-launcher-run-app
        "s-'" #'exwm-edit--compose)
  (after! exwm
    (dolist (key '(?\s-h ?\s-j ?\s-k ?\s-l ?\s-H ?\s-J ?\s-K ?\s-L ?\s-0 ?\s-1
                   ?\s-2 ?\s-3 ?\s-4 ?\s-5 ?\s-6 ?\s-7 ?\s-8 ?\s-9 ?\s-d
                   ?\s-\; ?\s-v ?\s-' ?\C-\[ ?\s-Q))
      (cl-pushnew key exwm-input-prefix-keys))))

