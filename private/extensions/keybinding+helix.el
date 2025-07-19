;; ---------------------------------------------------------
;; 基于SPACE的键设置
;; ---------------------------------------------------------

;; Enable helix-mode by default in all buffers
;; (add-hook 'after-change-major-mode-hook #'helix-mode)
;; (add-hook 'doom-after-init-hook #'helix-mode)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-z") #'helix-mode-all)

(keymap-global-set "<f3>" #'magit-status)
(keymap-global-set "<f4>" #'dired)

(keymap-global-set "<f5>" #'+eval/buffer-or-region)
(keymap-global-set "<f6>" #'projectile-recentf-files)
(keymap-global-set "<f7>" #'projectile-find-regexp)
(keymap-global-set "<f8>" #'dired)
(keymap-global-set "<f9>" #'org-capture)

(keymap-global-set "<f11>" #'+lookup/implementations)
(keymap-global-set "<f12>" #'+lookup/definition)

(keymap-global-set "<f2>" #'+workspace/cycle)
(global-set-key (kbd "s-<tab>") #'+workspace/cycle)
(global-set-key (kbd "s-N") #'+workspace/new)
(global-set-key (kbd "s-Q") #'+workspace/delete)
(keymap-global-set "s-H" #'+workspace/switch-left)
(keymap-global-set "s-l" #'+workspace/switch-right)

(global-set-key (kbd "C-/") #'comment-line)
(global-set-key (kbd "C-d") #'duplicate-line)

(global-set-key (kbd "C-t") #'consult-theme)

;; ---------------------------------------------------------
;; 基于SPACE的键设置
;; Normal mode is the default mode. You can return to it by pressing ESC.

;; Movement
;; Key	Description	Command
;; h	Move left	helix-backward-char
;; l	Move right	helix-forward-char
;; j	Move down	helix-next-line
;; k	Move up	helix-previous-line
;; w	Move next word start	helix-forward-word-start
;; W	Move next WORD	helix-forward-long-word-start
;; e	Move word end	helix-forward-word-end
;; E	Move WORD end	helix-forward-long-word-end
;; b	Move previous word	helix-backward-word
;; B	Move previous WORD	helix-backward-long-word
;; t	Find 'till next char	helix-find-till-char
;; T	Find 'till prev char	helix-find-prev-till-char
;; f	Find next char	helix-find-next-char
;; F	Find prev char	helix-find-prev-char
;; M-.	Repeat last motion (f, t, F, T)	helix-find-repeat
;; G	Go to line	N/A
;; C-b	Move page up	N/A
;; C-f	Move page down	N/A
;; Changes
;; Key	Description	Command
;; d	Delete selection	helix-kill-thing-at-point
;; y	Yank selection	helix-kill-ring-save
;; p	Paste	N/A
;; v	Begin selection	helix-begin-selection
;; u	Undo	N/A
;; o	Insert newline	helix-insert-newline
;; O	Insert line above	helis-insert-prevline
;; i	Insert mode	helix-insert
;; I	Insert beginning of line	helix-insert-beginning-line
;; a	Insert after	helix-insert-after
;; A	Insert end of line	helix-insert-after-end-line
;; r	Replace with a character	helix-replace
;; R	Replace with yanked text	helix-replace-yanked
;; C-c	Comment line	N/A
;; Selection
;; Key	Description	Command
;; x	Select current line	helix-select-line
;; Search
;; Key	Description	Command
;; /	Search	helix-search
;; n	Continue search forwards	helix-search-forward
;; N	Continue search backwards	helix-search-backward
;; Command mode
;; Accessed by typing : in normal mode. Accepts typable commands like :write, :quit, and so on.

;; Goto mode
;; Accessed by typing g in normal mode.

;; Key	Description	Command
;; g	Go to beginning of file	helix-go-beginning-buffer
;; e	Go to end of file	helix-go-end-buffer
;; l	Go to end of line	helix-go-end-line
;; h	Go to beginning of line	helix-go-beginning-line
;; s	Go to first non-whitespace character	helix-go-first-nonwhitespace
;; r	Find references	N/A
;; d	Find definitions	N/A
;; Window mode
;; Accessed by typing C-w in normal mode.

;; Key	Description	Command
;; w	Switch to next window	N/A
;; v	Vertical right split	N/A
;; s	Horizontal bottom split	N/A
;; h	Move to left split	N/A
;; j	Move to split below	N/A
;; k	Move to split above	N/A
;; l	Move to right split	N/A
;; q	Close current window	N/A
;; o	Only keep current window	N/A
;; Space mode
;; Accessed by typing space in normal mode.

;; Key	Description	Command
;; f	Find file at project root	N/A
;; b	Switch to project buffer	N/A
;; j	Switch project	N/A
;; /	Search within project	N/A
;; ;; ---------------------------------------------------------

(use-package! helix
  :config
  (helix-jj-setup 0.2)

  (helix-define-key 'space " " #'execute-extended-command)
  (helix-define-key 'space ":" #'execute-extended-command)

  (helix-define-key 'space "b" #'consult-buffer)
  (helix-define-key 'space "f" #'projectile-find-file)
  (helix-define-key 'space "." #'consult-find)
  (helix-define-key 'space "/" #'project-find-regexp)

  (helix-define-key 'space "e" #'+eval/buffer-or-region)

  (helix-define-key 'space "s" #'consult-lsp-symbols)
  (helix-define-key 'space "d" #'consult-lsp-diagnostics)

  (helix-define-key 'space "x" #'save-buffer)
  (helix-define-key 'space "," #'doom/kill-other-buffers)

  (helix-define-key 'space "R" #'consult-theme)

  (helix-define-key 'goto "c" #'comment-line)

  (helix-define-key 'goto "]" #'next-buffer)
  (helix-define-key 'goto "[" #'previous-buffer)

  (helix-define-key 'space "t" #'+vterm/toggle)

  (helix-define-key 'space "v" #'split-window-right)
  (helix-define-key 'space "h" #'split-window-below)
  (helix-define-key 'space "q" #'delete-window)
  (helix-define-key 'space "o" #'delete-other-windows)
  (helix-define-key 'space "m" #'maximize-window)
  (helix-define-key 'window "m" #'maximize-window)

  (helix-define-typable-command "fmt" #'+format/region-or-buffer)

  (helix-define-key 'space "g" #'magit-status)

  ;; Set up default helix keybindings
  (helix-mode)
  (helix-mode-all)
  )

(use-package! helix
  :after multiple-cursors
  :config
  (helix-multiple-cursors-setup)
  ;; Ensure helix keymaps take precedence
  (setq emulation-mode-map-alists 
        (cons 'helix-mode-map (delq 'helix-mode-map emulation-mode-map-alists))))
