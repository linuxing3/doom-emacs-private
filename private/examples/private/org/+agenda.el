;;; private/org/+agenda.el -*- lexical-binding: t; -*-
;;
;; Super Org Agenda Configuration
;;
;; This file implements:
;; - Custom agenda views and commands
;; - Super agenda grouping logic  
;; - Todo state triggers and faces
;; - Agenda filtering hydra
;; - Tag alignment and display tweaks
;;
;; Key features:
;; - GTD-style workflow views
;; - Context-based task grouping
;; - Time-based filtering
;; - Project management
;; 
;; See README.org for usage overview.

;; Bootstrap org
(add-hook 'org-load-hook #'+org-private|setup-agenda t)
(add-hook 'org-load-hook #'+org-private|setup-todos t)
;;
;; Hooks
;;

(defun +org-private|setup-todos-addons ()
  (after! org
    ;;Todo tags
    (setq org-todo-state-tags-triggers
          (quote (("CANCELLED" ("CANCELLED" . t))
                  ("WAITING" ("WAITING" . t))
                  ("IN-PROGRESS" ("WAITING") ("IN-PROGRESS" . t))
                  (done ("WAITING") ("HOLD"))
                  ("TODO" ("WAITING") ("CANCELLED") ("IN-PROGRESS"))
                  ("NEXT" ("WAITING") ("CANCELLED") ("IN-PROGRESS"))
                  ("DONE" ("WAITING") ("CANCELLED") ("IN-PROGRESS")))))
    (bind-keys :map org-mode-map
               ("A-b" . (surround-text-with "+"))
               ("s-b" . (surround-text-with "*"))
               ("A-i" . (surround-text-with "/"))
               ("s-i" . (surround-text-with "/"))
               ("A-=" . (surround-text-with "="))
               ("s-=" . (surround-text-with "="))
               ("A-`" . (surround-text-with "~"))
               ("s-`" . (surround-text-with "~"))
               ("C-s-f" . forward-sentence)
               ("C-s-b" . backward-sentence))
    (font-lock-add-keywords            ; A bit silly but my headers are now
     'org-mode `(("^\\*+ \\(TODO\\) "  ; shorter, and that is nice canceled
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚑")
                            nil)))
                 ("^\\*+ \\(NEXT\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                            nil)))
                 ("^\\*+ \\(IN-PROGRESS\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "⚐")
                            nil)))
                 ("^\\*+ \\(CANCELED\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✘")
                            nil)))
                 ("^\\*+ \\(DONE\\) "
                  (1 (progn (compose-region (match-beginning 1) (match-end 1) "✔")
                            nil)))))
    )
  )

(defun +org-private|setup-todos ()
  "Setup todos ..."
  (after! org
    ;; (setq org-agenda-files '("~/Dropbox/org/"))
    (setq org-todo-keywords
          '((sequence "TODO" "NEXT" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))
    ;; todo keywords
    (setq org-todo-keyword-faces
          (quote (("TODO" :foreground "red" :weight bold)
                  ("NEXT" :foreground "blue" :weight bold)
                  ("IN-PROGRESS" :foreground "forest green" :weight bold)
                  ("WAITING" :foreground "orange" :weight bold)
                  ("DONE" :foreground "forest green" :weight bold)
                  ("CANCELLED" :foreground "forest green" :weight bold)
                  )))

    )
  )
(defun +org-private|setup-agenda ()
  (setq org-agenda-block-separator ""
        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 3 :fileskip0 t :stepskip0 t :tags "-COMMENT"))
        org-agenda-compact-blocks t
        org-agenda-dim-blocked-tasks nil
        org-agenda-follow-indirect t
        org-agenda-ignore-properties '(effort appt category)
        org-agenda-inhibit-startup t
        org-agenda-log-mode-items '(closed clock)
        org-agenda-overriding-header ""
        org-agenda-restore-windows-after-quit t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-deadline-prewarning-if-scheduled t
        org-agenda-skip-unavailable-files t
        org-agenda-sorting-strategy '((agenda time-up priority-down category-keep)
                                      (todo   priority-down category-keep)
                                      (tags   priority-down category-keep)
                                      (search category-keep))
        org-agenda-span 'day
        org-agenda-start-with-log-mode t
        org-agenda-sticky nil
        org-agenda-tags-column 'auto
        org-agenda-use-tag-inheritance nil
        org-habit-following-days 0
        org-habit-graph-column 1
        org-agenda-tags-todo-honor-ignore-options t
        org-habit-preceding-days 8
        org-habit-show-habits t
        ))
;;
;; Plugins
;;
(def-package! org-super-agenda
  :commands (org-super-agenda-mode)
  :config
  ;; Super agenda Examples
  ;; (:auto-group t)
  ;; (:auto-category t)
  ;; (:name "马上去做 Quick Picks"
  ;;        :effort< "0:30")
  ;; (:name "下一任务 Next Items"
  ;;        :time-grid t)
  ;; (:name "重要任务 Important"
  ;;        :priority "A")
  ;; (:priority<= "B"
  ;;              :scheduled today
  ;;              :order 1)
  ;; (:habit t)
  ;; (:name "综合项目 Projects"
  ;;        :children t)
  ;; (:todo "TODO" :order 8)  ; Set order of this section
  ;; (:todo ("NEXT" "IN-PROGRESS" "WAITING") :order 9)
  ;; (:name "office" :tag "@office")
  ;; (:name "home" :tag "@home")
  ;; (:name "errand" :tag "@errand")
  ;; (:name "travel" :tag "@travel")
  ;; (:name "Today"
  ;;        :scheduled today)
  ;; (:name "完成任务 Done Items"
  ;;        :todo ("DONE" "CANCELLED"))
  ;; (:order-multi (1 (:name "Done today"
  ;;                        :and (:regexp "State \"DONE\"" :log t))
  ;;                 (:name "Clocked today"
  ;;                        :log t)
  ;; (:name "今日到期 Due today"
  ;;        :deadline today)
  ;; (:discard (:anything t))
  )

(after! org-agenda
  (setq org-agenda-sticky t)
  (setq org-stuck-projects (quote ("" nil nil "")))
  ;; Enable super agenda
  (org-super-agenda-mode)
  ;; Agenda filter
  (defhydra +org@org-agenda-filter (:color pink :hint nil)
    "
_;_ tag      _h_ headline      _c_ category     _r_ regexp     _d_ remove    "
    (";" org-agenda-filter-by-tag)
    ("h" org-agenda-filter-by-top-headline)
    ("c" org-agenda-filter-by-category)
    ("r" org-agenda-filter-by-regexp)
    ("d" org-agenda-filter-remove-all)
    ("q" nil "cancel" :color blue))
  ;; align tags
  (defun org-agenda-align-tags (&optional line)
    "Align all tags in agenda items to `org-agenda-tags-column'."
    (let ((inhibit-read-only t)
          (org-agenda-tags-column (if (eq 'auto org-agenda-tags-column)
                                      (- (- (window-text-width) 2))
                                    org-agenda-tags-column))
          l c)
      (save-excursion
        (goto-char (if line (point-at-bol) (point-min)))
        (while (re-search-forward "\\([ \t]+\\)\\(:[[:alnum:]_@#%:]+:\\)[ \t]*$"
                                  (if line (point-at-eol) nil) t)
          (add-text-properties
           (match-beginning 2) (match-end 2)
           (list 'face (delq nil (let ((prop (get-text-property
                                              (match-beginning 2) 'face)))
                                   (or (listp prop) (setq prop (list prop)))
                                   (if (memq 'org-tag prop)
                                       prop
                                     (cons 'org-tag prop))))))
          (setq l (- (match-end 2) (match-beginning 2))
                c (if (< org-agenda-tags-column 0)
                      (- (abs org-agenda-tags-column) l)
                    org-agenda-tags-column))
          (delete-region (match-beginning 1) (match-end 1))
          (goto-char (match-beginning 1))
          (insert (org-add-props
                      (make-string (max 1 (- c (current-column))) ?\ )
                      (plist-put (copy-sequence (text-properties-at (point)))
                                 'face nil))))
        (goto-char (point-min))
        (org-font-lock-add-tag-faces (point-max)))))
  ;; Popup window
  (set! :popup "^\\*Org Agenda.*\\*$" '((slot . -1) (size . 0.5) (side . right)) '((select . t) (modeline . nil)))
  ;; Tweatks
  ;; (push 'org-agenda-mode evil-snipe-disabled-modes)
  (add-hook 'org-agenda-finalize-hook #'hide-mode-line-mode)
  (set! :evil-state 'org-agenda-mode 'normal))

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
