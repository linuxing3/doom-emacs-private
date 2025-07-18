;;; lang-x/javascript/+debug.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skewer mode for testing
;; ~/.emacs.d/.local/packages/elpa/skewer-mode-20170730.1241/skewer-mode.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! skewer-mode
  :after js2-mode
  :hook (js2-mode . skewer-mode)
  :commands (skewer-mode run-skewer)
  :config
  (defun my-skewer-start-repl ()
    "Attach a browser to Emacs and start a skewer REPL."
    (interactive)
    (run-skewer)
    (skewer-repl))
  (defun my-skewer-load-buffer-and-focus ()
    "Execute whole buffer in browser and switch to REPL in insert state."
    (interactive)
    (skewer-load-buffer)
    (skewer-repl)
    (evil-insert-state))
  (defun my-skewer-eval-defun-and-focus ()
    "Execute function at point in browser and switch to REPL in insert state."
    (interactive)
    (skewer-eval-defun)
    (skewer-repl)
    (evil-insert-state))
  (defun my-skewer-eval-region (beg end)
    "Execute the region as JavaScript code in the attached browser."
    (interactive "r")
    (skewer-eval (buffer-substring beg end) #'skewer-post-minibuffer))
  (defun my-skewer-eval-region-and-focus (beg end)
    "Execute the region in browser and swith to REPL in insert state."
    (interactive "r")
    (spacemacs/skewer-eval-region beg end)
    (skewer-repl)
    (evil-insert-state)))
