;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; ---------------------------------------------------------
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; ---------------------------------------------------------
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; ---------------------------------------------------------
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; ---------------------------------------------------------
(setq user-full-name "Xing Wenju"
      user-mail-address "linuxing3@qq.com")

;; FIXME Moved to autoload
;; (load! "extensions/utils" nil t)
;; ---------------------------------------------------------
;; UI
;; ---------------------------------------------------------
;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Hack" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; Simple tweaks
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist
             '(ns-appearance . dark))
(setq doom-scratch-buffer-major-mode 'org-mode
      treemacs-width 32

      ;; Line numbers are pretty slow all around. The performance boost of
      ;; disabling them outweighs the utility of always keeping them on.
      display-line-numbers-type nil

      ;; On-demand code completion. I don't often need it.
      company-idle-delay nil

      ;; lsp-ui-sideline is redundant with eldoc and much more invasive, so
      ;; disable it by default.
      lsp-ui-sideline-enable nil
      lsp-enable-indentation nil
      lsp-enable-on-type-formatting nil
      lsp-enable-symbol-highlighting nil
      lsp-enable-file-watchers nil

      ;; Disable help mouse-overs for mode-line segments (i.e. :help-echo text).
      ;; They're generally unhelpful and only add confusing visual clutter.
      mode-line-default-help-echo nil
      show-help-function nil)

(setq dired-hide-details-mode t)
;; Auto revert-mode. Look ma, no hands...
(setq global-auto-revert-mode t)
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
;; All coding utf-8
;; (set-terminal-coding-system 'utf-8)
;; (set-language-environment 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (setq locale-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;;
;;
;;; :editor evil
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; set column whitespace style
(setq
 whitespace-line-column 100
 whitespace-style
 '(face trailing lines-tail))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; (load! "extensions/ui+font" nil t)

;; ---------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. 
;; It must be set before org loads!
(setq org-directory (dropbox-path "org"))
;; It can be set after org loads!
(load! "extensions/org+config" nil t)
(load! "extensions/org+capture" nil t)
(load! "extensions/org+agenda" nil t)
(load! "extensions/org+brain" nil t)
(load! "extensions/org+roam" nil t)
(load! "extensions/org+babel" nil t)

;; ---------------------------------------------------------
;; Languages Support
;; ---------------------------------------------------------
(load! "extensions/lang+python" nil t)
;; ---------------------------------------------------------
;; App
;; ---------------------------------------------------------
(load! "extensions/app+blog" nil t)
(load! "extensions/app+plantuml" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
