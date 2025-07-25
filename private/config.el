(setq user-full-name "linuxing3"
      user-mail-address "linuxing3@qq.com")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'semi-light))
(setq doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 18))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))
;;
(setq doom-theme 'doom-dark+)

(setq projectile-default-src-directory (expand-file-name "~/sources"))
(setq projectile-project-search-path 'projectile-default-src-directory)

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

(setq display-line-numbers-type t)

;; ---------------------------------------------------------
;; Org mode
;; ---------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'.
;; It must be set before org loads!
;;
(defvar org-directory-default nil
  "whether use org directory in default location")

(if org-directory-default
    (setq org-directory (expand-file-name "~/org"))
  (setq org-directory (dropbox-path "org")))

(load! "extensions/default" nil t)
;; These can be set after org loads!
(load! "extensions/org+config" nil t)
(load! "extensions/org+capture" nil t)
(load! "extensions/org+agenda" nil t)
(load! "extensions/org+babel" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
;; (load! "extensions/keybinding+spacemacs" nil t)
(load! "extensions/keybinding+helix" nil t)

;; Load windows manger extra settings
(load! "extensions/exwm" nil t)

;; read rss with org
;; (load! "extensions/feed" nil t)
