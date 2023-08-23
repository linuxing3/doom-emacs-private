;;; private/keybinding/+org-binding.el -*- lexical-binding: t; -*-

;;;; Org Map Key Bindings

(define-prefix-command 'org-map)
(define-key org-map (kbd "1") 'org-mode) ;; add brain
(define-key org-map (kbd "2") 'org-agenda) ;; agenda
(define-key org-map (kbd "3") 'cfw:open-org-calendar) ;; org calendar view
(define-key org-map (kbd "4") 'org-capture) ;; capture
(define-key org-map (kbd "5") 'org-refile) ;; refile
(define-key org-map (kbd "6") 'org-archive-subtree) ;; Archive subtree
(define-key org-map (kbd "7") 'org-priority) ;; priority
(define-key org-map (kbd "8") 'org-pomodoro) ;; add pomodoro clock 25
(define-key org-map (kbd "9") 'pomidor) ;; add pomodoro clock 25
(define-key org-map (kbd "i") 'org-clock-in) ;; clock in
(define-key org-map (kbd "o") 'org-clock-out) ;; clock out
(define-key org-map (kbd "t") 'org-todo) ;; todo
(define-key org-map (kbd "n") 'org-add-note) ;; add note
(define-key org-map (kbd "b") 'org-brain) ;; add brain
;; org mode map
(define-key org-mode-map (kbd "RET")  #'my/org-return)
