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
;; emacs escape as escape
(map! "<escape>" 'keyboard-escape-quit)

(map! "C-\\" #'doom/escape)

;; open doom private config
(map! "C-," #'doom/open-private-config)

;; envrc allow virtual develop environment
(map! "C-." #'envrc-allow)

;; helix mode for all buffers
(map! "C-<tab>" #'helix-mode-all)

;; quick switch buffers
(map! "M-<tab>" #'consult-buffer-other-window)

;; quick switch workspace
(map! "s-<tab>" #'+workspace/cycle)

;; open kitty, C-S-T for new tab, C-S-Q to exit
(defun x/open-kitty-new-workspace ()
  (interactive)
  (start-process-shell-command "kitty" nil "kitty"))
(defun x/open-kitty-here ()
  (interactive)
  (split-window-horizontally)
  (start-process-shell-command "kitty" nil "kitty"))
(map! "s-<return>" 'x/open-kitty-new-workspace)
(map! "M-<return>" 'x/open-kitty-here)

;; lsp rename
(map! "<f2>" #'lsp-rename)
;; git
(map! "<f3>" #'magit-status)

;; buffers and files
(map! "<f4>" #'dired)
(map! "<f5>" #'+eval/buffer-or-region)
(map! "<f6>" #'consult-buffer)
(map! "<f7>" #'consult-fd)
(map! "<f8>" #'consult-grep)

;; org capture
(map! "<f9>" #'org-capture)

;; lookup when coding
(map! "<f10>" #'+lookup/references)
(map! "<f11>" #'+lookup/implementations)
(map! "<f12>" #'+lookup/definition)

;; workspace
(map! "s-N" #'+workspace/new)
(map! "s-Q" #'+workspace/kill)

(map! "s-H" #'+workspace/switch-left)
(map! "s-L" #'+workspace/switch-right)

;; projects
(map! "s-p" #'projectile-switch-project)

;; windows
(map! "s-v" #'split-window-right)
(map! "s-d" #'split-window-below)
(map! "s-q" #'delete-window)

;; save buffer
(map! "s-s" #'save-buffer)

;; comment the way
(map! "C-/" #'comment-line)

;; repeat this line as in vscode
(map! "C-m" #'duplicate-line)
(map! "M-s-<down>" #'duplicate-line)

;; change what emacs looks like
(map! "C-t" #'consult-theme)

;; move between windows quickly
(map! "C-h" #'windmove-left)
(map! "C-l" #'windmove-right)
(map! "C-j" #'windmove-down)
(map! "C-k" #'windmove-up)

;; multi cursors
(map! "s-a" #'mc/edit-beginnings-of-lines)
(map! "s-e" #'mc/edit-ends-of-lines)

;; expand and multi select
(map! "C-d" #'mc/mark-next-word-like-this)

(map! "s-=" #'er/expand-region)
(map! "s-+" #'mc/mark-next-like-this)
(map! "s-*" #'mc/mark-all-like-this)

(map! "C-=" #'er/expand-region)
(map! "C-+" #'mc/mark-next-like-this)
(map! "C-*" #'mc/mark-all-like-this)

(map! "s-j" #'mc/mark-next-like-this)
(map! "M-s-j" #'mc/mark-next-like-this)
(map! "s-k" #'mc/mark-previous-like-this)
(map! "M-s-k" #'mc/mark-previous-like-this)

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
