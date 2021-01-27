;;; completion/autocomplete/autoload.el -*- lexical-binding: t; -*-


;;;###autoload
(defun +ac/toggle-auto-completion ()
  "Toggle as-you-type code completion."
  (interactive)
  (require 'auto-complete)
  (ac-config-default)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (setq ac-auto-start 4)
  (setq ac-auto-show-menu 0.8)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil)
  (setq ac-menu-height 19)
  (add-to-list 'ac-modes 'js2-mode)
  (add-to-list 'ac-modes 'js-mode)
  (set-face-background 'ac-candidate-face "lightgray")
  (set-face-underline 'ac-candidate-face "darkgray")
  (set-face-background 'ac-selection-face "steelblue")
  )
