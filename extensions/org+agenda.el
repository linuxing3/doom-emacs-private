;;; org+agenda.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;
(use-package! org-super-agenda
  :commands (org-super-agenda-mode)
  :config)

(after! org-agenda
  (setq org-agenda-custom-commands
        '(
          ;; My GTD tasks
          ("u"
           "My GTD view"
           (
            (todo "" (
                      (org-agenda-overriding-header "Get Things Done")
                      (org-super-agenda-groups
                       '(
                         (:name "马上去做 Quick Picks"
                                :effort< "0:30")
                         (:name "重要任务 Important"
                                :priority "A")
                         (:priority<= "B"
                                      :scheduled today
                                      :order 1)
                         (:discard (:anything t))))))
            (todo "" (
                      (org-agenda-overriding-header "All Projects")
                      (org-super-agenda-groups
                       '(
                         (:name none  ; Disable super group header
                                :children todo)
                         (:discard (:anything t))))))))
          ;; My grouped tasks
          ("x"
           "My Super view"
           (
            (agenda "" (
                        (org-agenda-overriding-header "Today Calendar")
                        (org-super-agenda-groups
                         '(
                           (:name "Today"
                                  :time-grid t)))))))
          ;; Daniel's tasks
          ("d"
           "Daniel's Task view"
           (
            (todo "" (
                      (org-agenda-overriding-header "Daniel's Tasks")
                      (org-super-agenda-groups
                       '(
                         (:name "daniel" :tag ("DANIEL" "daniel" "kids" "KIDS"))
                         (:discard (:anything t))))))))
          ;; End
          ("e"
           "Computer Related"
           (
            (tags-todo "" (
                      (org-agenda-overriding-header "Computer Related")
                      (org-super-agenda-groups
                       `(
                         (:name "General Comupter Related"
                                :tag "COMPUTER"
                                )
                         (:name "Emacs Related"
                                :tag "COMPUTER"
                                :regexp ("org" "emacs" ,(rx bow "emacs" eow))
                                )
                         )))))))))
