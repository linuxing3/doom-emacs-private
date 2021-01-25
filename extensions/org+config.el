;;; org+config.el -*- lexical-binding: t; -*-
;;;
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Hooks
(add-hook 'org-mode-hook #'auto-fill-mode)

(after! org
  ;; Add built-in modules of org
  (setq
   org-modules (quote (org-bibtex org-habit org-protocol org-mac-link))
   org-ellipsis " ▼ "
   org-bullets-bullet-list '(" ○" " ◆")
   org-tags-column -80
   )

  ;; Some directories' location
  (setq diary-file (dropbox-path "org/diary"))
  ;; Add all files under org-directory folder to agenda
  (setq
   org-agenda-diary-file (dropbox-path "org/diary")
   org-agenda-files (directory-files org-directory t "\\.org$" t))

  ;; Customize archived location
  (setq org-archive-location (dropbox-path "org/archived/%s_archive::"))

  ;; RSS with Elfeed
  (setq rmh-elfeed-org-files (list
                              (concat org-directory "/elfeed1.org")
                              (concat org-directory "/elfeed2.org")))
  (setq elfeed-db-directory (concat org-directory "/elfeed/db/"))
  (setq elfeed-enclosure-default-dir (concat org-directory "/elfeed/enclosures/"))
  (setq elfeed-search-filter "@3-month-ago +unread")

  ;; Stuck projects
  (setq org-stuck-projects
        '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

  ;; Config refile targets
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '((nil :maxlevel . 4)
          (org-agenda-files :maxlevel . 4)))

  ;; Column View
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))))
