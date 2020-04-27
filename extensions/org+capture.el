;;; org+capture.el -*- lexical-binding: t; -*-

(after! org

  (org-add-link-type
   "grep" 'endless/follow-grep-link)

  (defun endless/follow-grep-link (regexp)
    "Run `rgrep' with REGEXP as argument."
    (grep-compute-defaults)
    (rgrep regexp "*" (expand-file-name "./")))

  (org-add-link-type
   "yt" 'endless/follow-youtube-link)

  (defun endless/follow-youtube-link (id)
    "Run `rgrep' with REGEXP as argument."
    (browse-url (concat "https://www.youtube.com/watch?v=" id)))

  (org-add-link-type
   "tag" 'endless/follow-tag-link)

  (defun endless/follow-tag-link (tag)
    "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
    (org-tags-view (null current-prefix-arg) tag))

  (defun get-year-and-month ()
    (list (format-time-string "%Y年") (format-time-string "%m月")))


  (defun find-month-tree ()
    (let* ((path (get-year-and-month))
           (level 1)
           end)
      (unless (derived-mode-p 'org-mode)
        (error "Target buffer \"%s\" should be in Org mode" (current-buffer)))
      (goto-char (point-min))           ;移动到 buffer 的开始位置
      ;; 先定位表示年份的 headline，再定位表示月份的 headline
      (dolist (heading path)
        (let ((re (format org-complex-heading-regexp-format
                          (regexp-quote heading)))
              (cnt 0))
          (if (re-search-forward re end t)
              (goto-char (point-at-bol)) ;如果找到了 headline 就移动到对应的位置
            (progn                       ;否则就新建一个 headline
              (or (bolp) (insert "\n"))
              (if (/= (point) (point-min)) (org-end-of-subtree t t))
              (insert (make-string level ?*) " " heading "\n"))))
        (setq level (1+ level))
        (setq end (save-excursion (org-end-of-subtree t t))))
      (org-end-of-subtree)))


  (defun random-alphanum ()
    (let* ((charset "abcdefghijklmnopqrstuvwxyz0123456789")
           (x (random 36)))
      (char-to-string (elt charset x))))

  (defun create-password ()
    (let ((value ""))
      (dotimes (number 16 value)
        (setq value (concat value (random-alphanum))))))


  (defun get-or-create-password ()
    (setq password (read-string "Password: "))
    (if (string= password "")
        (create-password)
      password))

  (defun org-capture-template-goto-link ()
    (org-capture-put :target (list 'file+headline
                                   (nth 1 (org-capture-get :target))
                                   (org-capture-get :annotation)))
    (org-capture-put-target-region-and-position)
    (widen)
    (let ((hd (nth 2 (org-capture-get :target))))
      (goto-char (point-min))
      (if (re-search-forward
           (format org-complex-heading-regexp-format (regexp-quote hd)) nil t)
          (org-end-of-subtree)
        (goto-char (point-max))
        (or (bolp) (insert "\n"))
        (insert "* " hd "\n"))))

  (defun generate-anki-note-body ()
    (interactive)
    (message "Fetching note types...")
    (let ((note-types
           (sort (anki-editor--anki-connect-invoke-result "modelNames" 5)
                 #'string-lessp))
          note-type fields)
      (setq note-type (completing-read "Choose a note type: " note-types))
      (message "Fetching note fields...")
      (setq fields (anki-editor--anki-connect-invoke-result
                    "modelFieldNames" 5
                    `((modelName . ,note-type))))
      (concat "  :PROPERTIES:\n"
              "  :ANKI_NOTE_TYPE: " note-type "\n"
              "  :END:\n\n"
              (mapconcat (lambda (str) (concat "** " str))
                         fields
                         "\n\n"))))
  ;; FIXME capture chrome https://www.diegoberrocal.com/blog/2015/08/19/org-protocol/
  (defadvice org-capture
      (after make-full-window-frame activate)
    "Advise capture to be the only window when used as a popup"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (if (equal "emacs-capture" (frame-parameter nil 'name))
        (delete-frame)))

  ;; Capture template

  ;; (setq org-capture-templates nil)

  (add-to-list 'org-capture-templates '("x" "Extra"))

  (setq anki-org-file (dropbox-path "org/anki.org"))
  (add-to-list 'org-capture-templates
               `("xv"
                 "Vocabulary"
                 entry
                 (file+headline anki-org-file "Vocabulary")
                 ,(concat "* %^{heading} :note:\n"
                          "%(generate-anki-note-body)\n")))
  (setq snippets-org-file (dropbox-path "org/snippets.org"))
  (add-to-list 'org-capture-templates
               '("xs"
                 "Snippets"
                 entry
                 (file snippets-org-file)
                 (file "~/.doom.d/templates/capture-template/snippet.template")
                 ;; "* %?\t%^g\n #+BEGIN_SRC %^{language}\n\n#+END_SRC"
                 :kill-buffer t))
  (setq billing-org-file (dropbox-path "org/billing.org"))
  (add-to-list 'org-capture-templates
               '("xb"
                 "Billing"
                 plain
                 (file+function billing-org-file find-month-tree)
                 (file "~/.doom.d/templates/capture-template/billing.template")
                 ;; " | %U | %^{类别} | %^{描述} | %^{金额} |"
                 :kill-buffer t))

  (setq contacts-org-file (dropbox-path "org/contacts.org"))
  (add-to-list 'org-capture-templates
               '("xc"
                 "Contacts"
                 entry
                 (file contacts-org-file)
                 (file "~/.doom.d/templates/capture-template/contact.template")
                 ;; "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p %^{微信}p %^{微博}p %^{whatsapp}p\n\n  %?"
                 :empty-lines 1 :kill-buffer t))

  (setq password-org-file (dropbox-path "org/password.org.cpt"))
  (add-to-list 'org-capture-templates
               '("xp"
                 "Passwords"
                 entry
                 (file "D:/Dropbox/passwords.org.cpt")
                 "* %U - %^{title} %^G\n\n  - 用户名: %^{用户名}\n  - 密码: %(get-or-create-password)"
                 :empty-lines 1 :kill-buffer t))

  (setq blog-org-file (dropbox-path "org/blog.org"))
  (add-to-list 'org-capture-templates
               `("xx"
                 "Blog"
                 plain
                 (file ,(concat blog-org-file (format-time-string "%Y-%m-%d.org")))
                 ,(concat "#+startup: showall\n"
                          "#+options: toc:nil\n"
                          "#+begin_export html\n"
                          "---\n"
                          "layout     : post\n"
                          "title      : %^{标题}\n"
                          "categories : %^{类别}\n"
                          "tags       : %^{标签}\n"
                          "---\n"
                          "#+end_export\n"
                          "#+TOC: headlines 2\n")
                 ))

  ;; Protocol Group
  (setq links-org-file (dropbox-path "org/links.org"))
  (add-to-list 'org-capture-templates
               '("l"
                 "Temp Links from the interwebs"
                 entry
                 (file+headline links-org-file "Bookmarks")
                 "* %t %:description\nlink: %l \n\n%i\n"
                 :kill-buffer nil))

  (add-to-list 'org-capture-templates
               '("a"
                 "Protocol Annotation"
                 plain
                 (file+function links-org-file org-capture-template-goto-link)
                 " %^{Title}\n  %U - %?\n\n  %:initial"
                 :empty-lines 1))

  ;; Task Group
  (add-to-list 'org-capture-templates '("t" "Tasks"))

  (setq daniel-org-file (dropbox-path "org/daniel.org"))
  (add-to-list 'org-capture-templates
               '("ts"                                              ; hotkey
                 "Son Daniel's Task"                               ; title
                 entry                                             ; type
                 (file+headline daniel-org-file "Task") ; target
                 "** TODO %^{任务}\n%u\n%a\n"                      ; template
                 :clock-in t
                 :clock-resume t))
  (setq lulu-org-file (dropbox-path "org/lulu.org"))
  (add-to-list 'org-capture-templates
               '("tl"
                 "Wife Lulu's Task"
                 entry
                 (file+headline lulu-org-file "Task")
                 "** TODO %^{任务}\n%u\n%a\n"
                 :clock-in t
                 :clock-resume t))
  (setq my-org-file (dropbox-path "org/xingwenju.org"))
  (add-to-list 'org-capture-templates
               '("tr"
                 "My Book Reading Task"
                 entry
                 (file+headline my-org-file "Reading")
                 "** TODO %^{书名}\n%u\n%a\n"
                 :clock-in t
                 :immediate-finish t
                 :clock-resume t))
  (setq projects-org-file (dropbox-path "org/projects.org"))
  (add-to-list 'org-capture-templates
               '("tp"
                 "My Work Projects"
                 entry
                 (file projects-org-file)
                 (file "~/.doom.d/templates/capture-template/project.template")
                 :empty-line 1
                 :clock-resume t))
  (setq works-org-file (dropbox-path "org/works.org"))
  (add-to-list 'org-capture-templates
               '("tw"
                 "My Work Task"
                 entry
                 (file+headline works-org-file "Work")
                 "** TODO %^{任务}\n%u\n%a\n"
                 :immediate-finish t
                 :clock-in t :clock-resume t))

  ;; Most often used"
  (setq phone-org-file (dropbox-path "org/inbox.org"))
  (add-to-list 'org-capture-templates
               '("P"
                 "My Phone calls"
                 entry
                 (file+headline phone-org-file "Phone Calls")
                 (file "~/.doom.d/templates/capture-template/phone.template")
                 ;; "* %^{Habit cards|music|balls|games}\n  %?"
                 :immediate-finish t
                 :new-line 1))

  (setq habit-org-file (dropbox-path "org/habit.org"))
  (add-to-list 'org-capture-templates
               '("h"
                 "My Habit"
                 entry
                 (file habit-org-file)
                 (file "~/.doom.d/templates/capture-template/habit.template")
                 ;; "* %^{Habit cards|music|balls|games}\n  %?"
                 :immediate-finish t
                 :new-line 1))

  (setq notes-org-file (dropbox-path "org/notes.org"))
  (add-to-list 'org-capture-templates
               '("n"
                 "My Notes"
                 entry
                 (file notes-org-file)
                 (file "~/.doom.d/templates/capture-template/notes.template")
                 ;; "* %^{Loggings For...} %t %^g\n  %?"
                 :immediate-finish t
                 :new-line 1))

  (setq inbox-org-file (dropbox-path "org/inbox.org"))
  (add-to-list 'org-capture-templates
               '("i"
                 "My GTD Inbox"
                 entry
                 (file inbox-org-file)
                 (file "~/.doom.d/templates/capture-template/inbox.template")
                 ;; "* [#%^{Priority}] %^{Title} %^g\n SCHEDULED:%U %?\n"
                 :immediate-finish t
                 :new-line 1))
  )
