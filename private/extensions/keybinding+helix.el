;;; private/extensions/keybinding+helix.el -*- lexical-binding: t; -*-
;;
;; ---------------------------------------------------------
;; 基于SPACE的键设置
;; ---------------------------------------------------------
;; Helix-style Keybindings
;;
;; This file provides Helix-inspired keybindings that match Spacemacs functionality.
;; Key principles:
;; - Modal editing with different states (normal/insert/visual)
;; - Space as leader key
;; - jk for escape
;; - Consistent with Spacemacs muscle memory
;;
;; See also:
;; - README.org for usage overview
;; - keybinding+spacemacs.el for original Spacemacs bindings

;; Enable helix-mode by default in all buffers
;;
;; escape to as escape
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-set-key (kbd "\\") #'doom/escape)

;; open doom private config
(global-set-key (kbd "C-,") #'doom/open-private-config)

;; envrc allow virtual develop env
(global-set-key (kbd "C-.") #'envrc-allow)

;; helix mode for all buffers
(global-set-key (kbd "C-<tab>") #'helix-mode-all)

(global-set-key (kbd "M-<tab>") #'consult-buffer-other-window)

(global-set-key (kbd "s-<tab>") #'+workspace/cycle)

;; open kitty, C-S-T for new tab, C-S-Q to exit
(defun x/open-kitty-new-workspace ()
  (interactive)
  (start-process-shell-command "kitty" nil "kitty"))
(defun x/open-kitty-here ()
  (interactive)
  (split-window-horizontally)
  (start-process-shell-command "kitty" nil "kitty"))
(global-set-key (kbd "s-<return>") 'x/open-kitty-new-workspace)
(global-set-key (kbd "M-<return>") 'x/open-kitty-here)

;; lsp rename
(keymap-global-set "<f2>" #'lsp-rename)
;; git
(keymap-global-set "<f3>" #'magit-status)

;; buffers and files
(keymap-global-set "<f4>" #'dired)
(keymap-global-set "<f5>" #'+eval/buffer-or-region)
(keymap-global-set "<f6>" #'consult-buffer)
(keymap-global-set "<f7>" #'consult-fd)
(keymap-global-set "<f8>" #'consult-grep)

;; org capture
(keymap-global-set "<f9>" #'org-capture)

;; lookup when coding
(keymap-global-set "<f10>" #'+lookup/references)
(keymap-global-set "<f11>" #'+lookup/implementations)
(keymap-global-set "<f12>" #'+lookup/definition)

;; workspace

(global-set-key (kbd "s-N") #'+workspace/new)
(global-set-key (kbd "s-Q") #'+workspace/kill)

(keymap-global-set "s-H" #'+workspace/switch-left)
(keymap-global-set "s-L" #'+workspace/switch-right)
(keymap-global-set "s-p" #'projectile-switch-project)

(global-set-key (kbd "s-v") #'split-window-right)
(global-set-key (kbd "s-d") #'split-window-below)
(global-set-key (kbd "s-q") #'delete-window)

(keymap-global-set "s-s" #'save-buffer)

;; comment the way
(global-set-key (kbd "C-/") #'comment-line)

;; repeat this line as in vscode
(global-set-key (kbd "C-d") #'duplicate-line)

;; change what emacs looks like
(global-set-key (kbd "C-t") #'consult-theme)

;; move between windows quickly
(global-set-key (kbd "C-h") #'windmove-left)
(global-set-key (kbd "C-l") #'windmove-right)
(global-set-key (kbd "C-j") #'windmove-down)
(global-set-key (kbd "C-k") #'windmove-up)

;; multi cursors
(global-set-key (kbd "S-s-a") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "S-s-e") #'mc/edit-ends-of-lines)

(global-set-key (kbd "S-s-c") #'mc/mark-next-like-this)
(global-set-key (kbd "S-s-b") #'mc/mark-previous-like-this)

(global-set-key (kbd "S-s-<down>") #'mc/mark-next-like-this)
(global-set-key (kbd "s-j") #'mc/mark-next-like-this)
(global-set-key (kbd "S-s-<up>") #'mc/mark-previous-like-this)
(global-set-key (kbd "s-k") #'mc/mark-previous-like-this)

;; ---------------------------------------------------------
;; 基于SPACE的键设置
;; ---------------------------------------------------------

(use-package! helix
  :config
  (helix-jj-setup 0.2)

  (helix-define-key 'space " " #'execute-extended-command)

  ;; lsp actions
  (helix-define-key 'goto "d" #'+lookup/definition)
  (helix-define-key 'goto "i" #'+lookup/implementations)
  (helix-define-key 'goto "r" #'+lookup/references)
  (helix-define-key 'goto "y" #'+lookup/type-definition)

  ;; quick finder
  (helix-define-key 'goto "." #'consult-find)   ;;
  (helix-define-key 'goto "b" #'consult-buffer) ;;
  (helix-define-key 'goto "f" #'consult-fd)     ;;
  (helix-define-key 'goto "/" #'consult-grep)   ;;

  ;; multi cursor
  (helix-define-key 'goto "0" #'mc/mark-all-words-like-this)
  (helix-define-key 'goto "9" #'mc/mark-all-like-this-dwim)

  ;; select allow
  (helix-define-key 'goto "8" #'mc/mark-all-like-this)
  (helix-define-key 'goto "*" #'mc/mark-all-like-this)
  (helix-define-key 'space "8" #'mc/mark-all-like-this)
  (helix-define-key 'space "*" #'mc/mark-all-like-this)

  ;; project
  (helix-define-key 'space "f" #'projectile-find-file)
  (helix-define-key 'space "b" #'projectile-switch-to-buffer)
  (helix-define-key 'space "j" #'projectile-switch-project)
  (helix-define-key 'space "\\" #'project-find-regexp)

  ;; default lsp
  (helix-define-key 'space "a" #'eglot-code-action-quickfix)
  (helix-define-key 'space "r" #'eglot-rename)
  (helix-define-key 'space "d" #'flymake-show-buffer-diagnostics)

  ;; git
  (helix-define-key 'space "g" #'magit-status)

  ;; eval
  (helix-define-key 'space "e" #'+eval/buffer-or-region)

  ;; lsp coding, a for action
  (helix-define-key 'space "s" #'consult-lsp-symbols)
  (helix-define-key 'space "d" #'consult-lsp-diagnostics)

  ;; saving buffers
  (helix-define-key 'space "x" #'save-buffer)
  (helix-define-key 'space "X" #'save-some-buffers)
  (helix-define-key 'space "z" #'+workspace/kill-session-and-quit)

  ;; only keey me
  (helix-define-key 'space "," #'doom/kill-other-buffers)

  ;; quick comment
  (helix-define-key 'goto "c" #'comment-line)

  ;; clangd switch header/source file
  (helix-define-key 'goto "H" #'lsp-clangd-find-other-file)

  ;; switch buffers
  (helix-define-key 'goto "]" #'next-buffer)
  (helix-define-key 'goto "[" #'previous-buffer)

  ;; terminal to run command
  (helix-define-key 'space "t" #'+vterm/toggle)

  ;; windows manipulation
  (helix-define-key 'space "v" #'split-window-right)
  (helix-define-key 'space "h" #'split-window-below)
  (helix-define-key 'space "q" #'delete-window)
  (helix-define-key 'space "o" #'delete-other-windows)

  (helix-define-key 'space "m" #'maximize-window)
  (helix-define-key 'window "m" #'maximize-window)

  ;; typable command
  (helix-define-typable-command "fmt" #'+format/region-or-buffer)
  (helix-define-typable-command "save" #'save-some-buffers)
  (helix-define-typable-command "reset" #'+workspace/kill-session-and-quit)

  ;; Set up default helix keybindings
  (helix-mode)
  (helix-mode-all)
  )
