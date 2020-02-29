;;; org+config.el -*- lexical-binding: t; -*-
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))


;; Add built-in modules of org
(setq
 org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
 org-ellipsis " ▼ "
 org-bullets-bullet-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
 )

(setq diary-file (dropbox-path "org/diary"))
;; Add all files under org-directory folder to agenda
(setq
 org-agenda-diary-file (dropbox-path "org/diary")
 org-agenda-files (directory-files org-directory t "\\.org$" t))

;; Customize archived location
(setq org-archive-location (dropbox-path "org/archived/%s_archive::"))

;; Stuck projects
(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

;; Config refile targets
(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-targets
      '((nil :maxlevel . 4)
        (org-agenda-files :maxlevel . 4)))

(setq org-use-speed-commands t
      org-return-follows-link t
      org-hide-emphasis-markers t
      org-completion-use-ido t
      org-outline-path-complete-in-steps nil
      org-src-fontify-natively t ;; Pretty code blocks
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil ;; No code evaluation confirm
      )
;; Column View
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
(setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                    ("STYLE_ALL" . "habit"))))
