(setq user-full-name "linuxing3"
      user-mail-address "linuxing3@qq.com")

(setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 18 :weight 'semi-light))
(setq doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 18))
(setq doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 24))
;;
(setq doom-theme 'doom-one)

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

(setq display-line-numbers-type t)


;; set font for chinese characters

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

;; These can be set after org loads!
(load! "extensions/org+config" nil t)
(load! "extensions/org+capture" nil t)
(load! "extensions/org+agenda" nil t)
(load! "extensions/org+brain" nil t)
(load! "extensions/org+roam" nil t)
;; (load! "extensions/org+babel" nil t)

;; (load! "extensions/lang+js" nil t)
;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
