
(setq user-full-name "Xing Wenju"
      user-mail-address "linuxing3@qq.com")

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))
(setq doom-theme 'doom-dracula)

;; set font for chinese characters
(setq doom-font (font-spec :family "IBM Plex Mono" :size 14))
(set-fontset-font
 t
 '(#x4e00 . #x9fff)
 (cond
  ((string-equal system-type "windows-nt")
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "Microsoft JhengHei" (font-family-list)) "Microsoft JhengHei")
    ((member "SimHei" (font-family-list)) "SimHei")))
  ((string-equal system-type "darwin")
   (cond
    ((member "Hei" (font-family-list)) "Hei")
    ((member "Heiti SC" (font-family-list)) "Heiti SC")
    ((member "Heiti TC" (font-family-list)) "Heiti TC")))
  ((string-equal system-type "gnu/linux")
   (cond
    ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei")))))


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
(load! "extensions/org+pretty" nil t)
(load! "extensions/org+capture" nil t)
(load! "extensions/org+agenda" nil t)
(load! "extensions/org+brain" nil t)
(load! "extensions/org+roam" nil t)
;; (load! "extensions/org+babel" nil t)

;; ---------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------
(load! "extensions/keybinding+spacemacs" nil t)
