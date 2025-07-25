(setq user-full-name "linuxing3"
      user-mail-address "linuxing3@qq.com")

(setq doom-theme 'doom-dark+)

(setq projectile-default-src-directory (expand-file-name "~/sources"))
(setq projectile-project-search-path 'projectile-default-src-directory)

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

(defvar org-directory-default nil
  "whether use org directory in default location")

(if org-directory-default
    (setq org-directory (expand-file-name "~/org"))
  (setq org-directory (dropbox-path "org")))

;; Better defaults
(load! "extensions/default" nil t)
