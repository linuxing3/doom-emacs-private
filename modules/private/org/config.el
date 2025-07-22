;;; private/org/config.el -*- lexical-binding: t; -*-
;; 
;; Org Mode Configuration
;;
;; This file configures core Org mode functionality including:
;; - File locations and directory structure
;; - Todo state workflow
;; - Clocking and time tracking
;; - Capture templates and refiling
;; - Export settings
;; - Custom hooks and advice
;; 
;; See README.org for overview documentation.

(setq org-directory (os-path "~/Dropbox/org"))
(setq org-journal-dir (os-path "~/Dropbox/org/journal"))
;; FIXME deprecated
(setq +org-dir (os-path "~/Dropbox/org"))

;; Sub-modules
(if (featurep! +babel)   (load! "+babel"))
(if (featurep! +latex)   (load! "+latex"))
(if (featurep! +brain)   (load! "+brain"))
(if (featurep! +trello)   (load! "+trello"))
(if (featurep! +export)  (load! "+export"))
(if (featurep! +present) (load! "+present"))
(if (featurep! +default-capture) (load! "+default-capture"))
(if (featurep! +quick-capture) (load! "+quick-capture"))
(if (featurep! +agenda) (load! "+agenda"))
(if (featurep! +bindings) (load! "+bindings"))

(defun +org-private|setup-customization ()
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
  ;; Add built-in modules of org
  (setq
   org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
   org-ellipsis " ▼ ")

  (setq diary-file "~/Dropbox/org/diary")
  ;; Add all files under org-directory folder to agenda
  (setq
   org-agenda-diary-file "~/Dropbox/org/diary"
   org-agenda-files (directory-files org-directory t "\\.org$" t))

  ;; Customize archived location
  (setq org-archive-location (os-path "~/Dropbox/org/archived/%s_archive::"))

  ;; Config refile targets
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '((nil :maxlevel . 4)
          (org-agenda-files :maxlevel . 4)))

  ;; Stuck projects
  (setq org-stuck-projects
        '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:")))

(defun +org-private|setup-misc ()
  ;; common
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
  ;; Default Properties
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))))

;;
;; Built-in libraries
(def-package! org-clock
  :init
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t))

;;; Org Capture
;;;; Thank you random guy from StackOverflow
;;;; http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection

(defun +org-private|setup-chrome-capture ()
  "Get capture frame setup"
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame))))

;;
;; Bootstrap

(add-hook! 'org-load-hook
  #'(+org-private|setup-customization
     +org-private|setup-chrome-capture
     +org-private|setup-misc))
;; In case org has already been loaded (or you're running `doom/reload')
(when (featurep 'org)
  (run-hooks 'org-load-hook))
