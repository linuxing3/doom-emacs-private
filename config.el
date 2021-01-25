;; ---------------------------------------------------------
;; Basic configuration
;; ---------------------------------------------------------
(setq user-full-name "Xing Wenju"
      user-mail-address "linuxing3@qq.com")

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

;; ---------------------------------------------------------
;; set theme
;; ---------------------------------------------------------
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "BlexMono NF" :size 18))

(defun linuxing3/dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'linuxing3/dired-mode-setup)

;; ---------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'.
;; It must be set before org loads!
(defvar org-directory-default nil
  "whether use org directory in default location")

(if org-directory-default
    (setq org-directory (expand-file-name "org" home-directory))
  (setq org-directory (dropbox-path "org")))

;; These can be set after org loads!
(load! "extensions/org+config" nil t)
(load! "extensions/org+pretty" nil t)
(load! "extensions/org+capture" nil t)
(load! "extensions/org+agenda" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
