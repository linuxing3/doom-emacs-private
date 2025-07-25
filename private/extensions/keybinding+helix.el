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

;; Modal editing setup
(map! "<escape>" 'keyboard-escape-quit)
(map! "C-\\" #'doom/escape)
(map! "C-g" #'keyboard-escape-quit) ; VSCode-style escape

;; ---------------------------------------------------------
;; 基于VSCODE的键设置
;; ---------------------------------------------------------
;; VSCode-style file/buffer commands
(map! "C-," #'doom/open-private-config)
(map! "C-." #'envrc-allow)
(map! "C-p" #'projectile-find-file) ; VSCode quick open

(map! "C-<tab>" #'consult-buffer) ; VSCode-style buffer switching
(map! "M-<tab>" #'consult-buffer-other-window)
(map! "s-<tab>" #'+workspace/cycle)

;; VSCode-style editor commands
(map! "C-s" #'save-buffer)
(map! "C-f" #'consult-line) ; Quick find in file
(map! "C-h" #'consult-outline) ; File outline

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

;; Enhanced function keys

(map! "<f2>" #'lsp-rename) ; VSCode rename
(map! "C-<f2>" #'lsp-find-references) ; VSCode find all references
(map! "<f3>" #'magit-status)
(map! "<f4>" #'dired)
(map! "<f5>" #'+eval/buffer-or-region)
(map! "<f6>" #'consult-buffer)
(map! "<f7>" #'consult-fd)
(map! "<f8>" #'consult-grep)
(map! "<f9>" #'org-capture)
(map! "<f10>" #'+lookup/references)
(map! "<f11>" #'+lookup/implementations)
(map! "<f12>" #'+lookup/definition)
(map! "S-<f12>" #'+lookup/type-definition)
(map! "C-<f12>" #'lsp-find-implementation) ; VSCode go to implementation

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
(map! "M-s-<down>" #'duplicate-line)

;; change what emacs looks like
(map! "C-t" #'consult-theme)

;; Enhanced window navigation (VSCode + Helix style)
(map! "C-h" #'windmove-left)
(map! "C-l" #'windmove-right)
(map! "C-j" #'windmove-down)
(map! "C-k" #'windmove-up)
(map! "M-h" #'windmove-left) ; Alternative navigation
(map! "M-l" #'windmove-right)
(map! "M-j" #'windmove-down)
(map! "M-k" #'windmove-up)
(map! "C-w h" #'windmove-left) ; VSCode-style window commands
(map! "C-w l" #'windmove-right)
(map! "C-w j" #'windmove-down)
(map! "C-w k" #'windmove-up)

;; Enhanced multi-cursor support (VSCode + Helix style)
(map! "C-d" #'mc/mark-next-word-like-this) ; VSCode add next match
(map! "C-S-d" #'mc/mark-previous-word-like-this) ; VSCode add previous match
(map! "s-d" #'mc/mark-all-words-like-this) ; VSCode select all matches
(map! "C-M-d" #'mc/mark-all-in-region) ; VSCode add cursors to selection
(map! "C-M-<mouse-1>" #'mc/add-cursor-on-click) ; Add cursor on click

;; Selection expansion
(map! "C-=" #'er/expand-region)
(map! "C--" #'er/contract-region) ; VSCode shrink selection
(map! "C-S-=" #'mc/mark-all-like-this) ; VSCode select all occurrences

;; Line manipulation
(map! "s-a" #'mc/edit-beginnings-of-lines)
(map! "s-e" #'mc/edit-ends-of-lines)
(map! "C-S-k" #'kill-whole-line) ; VSCode delete line
(map! "C-S-<up>" #'drag-stuff-up) ; VSCode move line up
(map! "C-S-<down>" #'drag-stuff-down) ; VSCode move line down
(map! "C-x C-o" #'open-line) ; Insert newline below
(map! "C-x C-S-o" #'open-line-above) ; Insert newline above
(map! "RET" #'newline-and-indent) ; VSCode rename

;; Delimiter transformation functions
(defun change-surrounded-delimiter (from to)
  "Change delimiters surrounding point from FROM to TO.
FROM and TO should be strings like \"(\" or \"{\"."
  (interactive)
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
    (when bounds
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (current-char (buffer-substring-no-properties beg (1+ beg)))
             (end-char (buffer-substring-no-properties (1- end) end)))
        (when (and (string= current-char from)
                   (string= end-char (cl-case (string-to-char from)
                                     (?\( ")")
                                     (?\[ "]")
                                     (?\{ "}")
                                     (?< ">")
                                     (t ""))))
          (goto-char beg)
          (delete-char 1)
          (insert to)
          (goto-char (1- end))
          (delete-char 1)
          (insert (cl-case (string-to-char to)
                    (?\( ")")
                    (?\[ "]")
                    (?\{ "}")
                    (?< ">")
                    (t ""))))))))

(defun change-parens-to-braces ()
  "Change surrounding parentheses to curly braces."
  (interactive)
  (change-surrounded-delimiter "(" "{"))

(defun change-braces-to-parens ()
  "Change surrounding curly braces to parentheses."
  (interactive)
  (change-surrounded-delimiter "{" "("))

(defun change-parens-to-brackets ()
  "Change surrounding parentheses to square brackets."
  (interactive)
  (change-surrounded-delimiter "(" "["))

(defun change-brackets-to-parens ()
  "Change surrounding square brackets to parentheses."
  (interactive)
  (change-surrounded-delimiter "[" "("))

;; Enhanced jump to matching delimiter function
(defun jump-to-matching-delimiter ()
  "Jump to the matching delimiter (parenthesis, bracket, brace, or HTML/XML tag)."
  (interactive)
  (let ((pos (point)))
    (cond
     ;; Standard delimiters
     ((looking-at "\\s(") (forward-sexp 1))
     ((looking-back "\\s)" 1) (backward-sexp 1))
     ((looking-at "\\s{") (forward-sexp 1))
     ((looking-back "\\s}" 1) (backward-sexp 1))
     ((looking-at "\\s[") (forward-sexp 1))
     ((looking-back "\\s]" 1) (backward-sexp 1))
     
     ;; HTML/XML tags
     ((looking-at "<[^/!]") ; Opening tag
      (when (search-forward-regexp ">" nil t)
        (unless (sgml-skip-tag-forward 1)
          (message "No matching closing tag found")
          (goto-char pos))))
     
     ((looking-back "</" 2) ; Closing tag
      (when (search-backward-regexp "<[^/]" nil t)
        (unless (sgml-skip-tag-backward 1)
          (message "No matching opening tag found")
          (goto-char pos))))
     
     (t (message "Not at a delimiter")))))

;; ---------------------------------------------------------
;; 基于SPACE的键设置
;; ---------------------------------------------------------
;; Helix mode
(map! "C-`" #'helix-normal-mode) ; VSCode quick open

(use-package! helix
  :config
  (helix-jj-setup 0.2)

  ;; normal
  (helix-define-key 'normal "H" #'previous-buffer)
  (helix-define-key 'normal "L" #'next-buffer)

  ;; delimiter operations
  (helix-define-key 'normal "mm" #'jump-to-matching-delimiter)
  (helix-define-key 'normal "m(" #'change-braces-to-parens)
  (helix-define-key 'normal "m)" #'change-parens-to-braces)
  (helix-define-key 'normal "m[" #'change-brackets-to-parens)
  (helix-define-key 'normal "m]" #'change-parens-to-brackets)

  ;; expand and contract
  (helix-define-key 'normal "v" #'er/expand-region)
  (helix-define-key 'normal "V" #'er/contract-region)

  ;; multi cursor mark
  (helix-define-key 'normal "C" #'mc/mark-next-like-this)

  ;; space mode
  (helix-define-key 'space " " #'execute-extended-command)

  ;; project
  (helix-define-key 'space "f" #'projectile-find-file)
  (helix-define-key 'space "b" #'projectile-switch-to-buffer)
  (helix-define-key 'space "j" #'projectile-switch-project)
  (helix-define-key 'space "\\" #'project-find-regexp)

  ;; default lsp
  (helix-define-key 'space "a" #'lsp-execute-code-action)
  (helix-define-key 'space "r" #'lsp-rename)

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

  ;; only kill me
  (helix-define-key 'space "," #'doom/kill-other-buffers)

  ;; kill and yank / copy and past
  (helix-define-key 'space "p" #'clipboard-yank)
  (helix-define-key 'space "y" #'clipboard-kill-ring-save)

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

  ;; quick comment
  (helix-define-key 'goto "c" #'comment-line)

  ;; clangd switch header/source file
  (helix-define-key 'goto "H" #'lsp-clangd-find-other-file)

  ;; switch buffers
  (helix-define-key 'goto "]" #'next-buffer)
  (helix-define-key 'goto "n" #'next-buffer)
  (helix-define-key 'goto "[" #'previous-buffer)
  (helix-define-key 'goto "p" #'previous-buffer)

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
