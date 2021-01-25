;;; private/writer/+bindings.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Windows系统下，重新绑定Window和Shift键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; 菜单应用键Menu/App key

(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; 左Win键Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; 右Win键Right Windows key

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mac OS 系统下，重新绑定Cmd，Option，Control和Function键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; on Linux, the menu/apps key syntax is <menu>
;; on Windows, the menu/apps key syntax is <apps>
;; make the syntax equal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn

  (global-set-key (kbd "C-S-p") 'execute-extended-command)
  (global-set-key (kbd "H-x") 'execute-extended-command)
  (global-set-key (kbd "H-:") 'execute-extended-command)

  (global-set-key (kbd "H-r") '+eval/buffer)

  (global-set-key (kbd "H-0") 'delete-window)
  (global-set-key (kbd "H-9") 'delete-other-windows)
  (global-set-key (kbd "H--") 'split-window-below)
  (global-set-key (kbd "H-=") 'split-window-right)

  (global-set-key (kbd "H-7") 'dired-jump)
  (global-set-key (kbd "H-8") 'ispell-word)

  (global-set-key (kbd "H-a") 'mark-whole-buffer) ; select whole buffer
  (global-set-key (kbd "H-t") 'beginning-of-buffer); top of buffer
  (global-set-key (kbd "H-b") 'end-of-buffer) ; bottom of buffer

  (global-set-key (kbd "H-q") 'evil-escape) ; global evil escape
  (global-set-key (kbd "H-g") 'evil-escape) ; global evil escape

  ;; make cursor movement keys under right hand's home-row.
  (global-set-key (kbd "H-i") 'previous-line) ; previous
  (global-set-key (kbd "H-j") 'backward-char) ; left to backward
  (global-set-key (kbd "H-k") 'next-line) ; next line
  (global-set-key (kbd "H-l") 'forward-char)  ; right to forward

  (global-set-key (kbd "H-SPC") 'set-mark-command) ; was just-one-space

  (global-set-key (kbd "H-c") 'recenter-top-bottom) ; center current line

  (global-set-key (kbd "H-p") 'query-replace) ; find and replace

  (global-set-key (kbd "H-s") 'save-buffer) ; find and replace

  (global-set-key (kbd "C-c w d") 'ha-switch-default-browser)

  (global-set-key (kbd "H-z") 'comment-dwim))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 自定义的组合键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;* Examples
;;** Example 1: text scale
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-zoom (global-map "<C-f1>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

;;** Example 2: move window splitter
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-splitter (global-map "C-M-s")
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right)))

;;** Example 3: jump to error
(when (bound-and-true-p hydra-examples-verbatim)
  (defhydra hydra-error (global-map "M-g")
    "goto-error"
    ("h" first-error "first")
    ("j" next-error "next")
    ("k" previous-error "prev")
    ("v" recenter-top-bottom "recenter")
    ("q" nil "quit")))

;;** Example 4: toggle rarely used modes
(when (bound-and-true-p hydra-examples-verbatim)
  (defvar whitespace-mode nil)
  (global-set-key
   (kbd "C-c C-v")
   (defhydra hydra-toggle-simple (:color blue)
     "toggle"
     ("j" js2-mode "js2")
     ("o" org-mode "org")
     ("e" emacs-lisp-mode "elisp")
     ("a" abbrev-mode "abbrev")
     ("d" toggle-debug-on-error "debug")
     ("f" auto-fill-mode "fill")
     ("t" toggle-truncate-lines "truncate")
     ("w" whitespace-mode "whitespace")
     ("q" nil "cancel"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 功能键
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Map for Zoomming

(map! [f2] #'text-scale-increase)

(map! [f3] #'text-scale-decrease)

;; Map for file explorer

(map! [f4] #'neotree-toggle)

(map! [f5] #'ibuffer)

;; Quick Load theme

(map! [f6] #'load-theme)

(map! [f7] #'package-install)

(map! [f8] #'w3m)

;; Map for org mode

(map! "C-c i" #'org-capture)
(map! [f9] #'org-capture)

(map! [f10] #'org-agenda)

(map! [f11] #'projectile-run-shell-command-in-root)

(map! [f12] #'+lookup/definition)
