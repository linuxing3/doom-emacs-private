(setq user-full-name "linuxing3"
      user-mail-address "linuxing3@qq.com")

(setq doom-theme 'doom-dark+)

(setq projectile-default-src-directory (expand-file-name "~/sources"))
(setq projectile-project-search-path 'projectile-default-src-directory)

(setq bookmark-default-file (dropbox-path "shared/emacs-bookmarks"))

(setq org-directory (dropbox-path "org"))

(setq diary-file (dropbox-path "org/diary"))

(setq
 org-agenda-diary-file (dropbox-path "org/diary")
 org-agenda-files (directory-files org-directory t "\\.agenda\\.org$" t))

(setq org-archive-location (dropbox-path "org/archived/%s_archive::"))

;; Better defaults
(load! "extensions/default" nil t)
