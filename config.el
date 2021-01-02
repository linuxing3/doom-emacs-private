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

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

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
(setq doom-font (font-spec :family "IBM Plex Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-dracula)

;; Simple tweaks
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . dark))

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      treemacs-width 32
      global-auto-revert-mode t
      evil-split-window-below t
      evil-vsplit-window-right t
      whitespace-line-column 100
      whitespace-style '(face trailing lines-tail)
      display-line-numbers-type nil)


(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; FIXME All coding utf-8, but still org date font are wrong
(set-terminal-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)

;; FIXME UI font with unicode error
(load! "extensions/ui+font" nil t)

;; ---------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'.
;; It must be set before org loads!
;;
(defvar org-directory-default t
  "whether use org directory in default location")

(if org-directory-default
    (setq org-directory (expand-file-name "~/org"))
  (setq org-directory (dropbox-path "org")))

;; These can be set after org loads!
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
(load! "extensions/feature+magit" nil t)
(load! "extensions/feature+lsp" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
