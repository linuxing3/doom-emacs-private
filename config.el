
(setq user-full-name "Xing Wenju"
      user-mail-address "linuxing3@qq.com")

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))
(setq doom-theme 'doom-dracula)
(setq doom-font (font-spec :family "IBM Plex Mono" :size 14))
;; set font for chinese characters

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
;; (load! "extensions/org+agenda" nil t)
(load! "extensions/org+brain" nil t)
(load! "extensions/org+roam" nil t)
;; (load! "extensions/org+babel" nil t)

(load! "extensions/lang+js" nil t)
;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
