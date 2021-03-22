;; ---------------------------------------------------------
;; Basic configuration
;; ---------------------------------------------------------
(setq user-full-name "Xing Wenju"
      user-mail-address "linuxing3@qq.com")

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

;; ---------------------------------------------------------
;; set theme
;; ---------------------------------------------------------
(load! "extensions/ui+font" nil t)

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
(load! "extensions/app+plantuml" nil t)

;; Writing blogs
(load! "extensions/app+blog" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)

;; ---------------------------------------------------------
;; Better Defaults
;; ---------------------------------------------------------
(load! "extensions/emacs+default" nil t)
(load! "extensions/feature+packages" nil t)
