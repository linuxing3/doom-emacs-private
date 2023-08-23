;;; private/org/+quick-capture.el -*- lexical-binding: t; -*-

(after! org

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

  ;; Publish
  (setq org-publish-project-alist
        '(("blog-org"
           :base-directory "~/Dropbox/org/blog/"
           :base-extension "org"
           :publishing-directory "~/Dropbox/xingwenju.com/github-pages/"
           :recursive t
           :htmlized-source t
           :section-numbers nil
           :publishing-function org-html-publish-to-html
           :headline-levels 4
           :html-extension "html"
           :body-only t             ; Only export section between <body> </body>
           :table-of-contents nil
           )
          ("blog-static"
           :base-directory "~/Dropbox/org/blog/"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
           :publishing-directory "~/Dropbox/xingwenju.com/github-pages/"
           :recursive t
           :publishing-function org-publish-attachment
           )
          ("blog" :components ("blog-org" "blog-static"))))

  ;; Capture template

  (setq org-capture-templates nil)

  (add-to-list 'org-capture-templates '("x" "Extra"))

  (add-to-list 'org-capture-templates
               `("xv"
                 "Vocabulary"
                 entry
                 (file+headline "~/Dropbox/org/anki.org" "Vocabulary")
                 ,(concat "* %^{heading} :note:\n"
                          "%(generate-anki-note-body)\n")))
  (add-to-list 'org-capture-templates
               '("xs"
                 "Snippets"
                 entry
                 (file "~/Dropbox/org/snippets.org")
                 (file "~/Dropbox/org/capture-template/snippet.template")
                 ;; "* %?\t%^g\n #+BEGIN_SRC %^{language}\n\n#+END_SRC"
                 :kill-buffer t))

  (add-to-list 'org-capture-templates
               '("xb"
                 "Billing"
                 plain
                 (file+function "~/Dropbox/org/billing.org" find-month-tree)
                 (file "~/Dropbox/org/capture-template/billing.template")
                 ;; " | %U | %^{类别} | %^{描述} | %^{金额} |"
                 :kill-buffer t))

  (add-to-list 'org-capture-templates
               '("xc"
                 "Contacts"
                 entry
                 (file "~/Dropbox/org/contacts.org")
                 (file "~/Dropbox/org/capture-template/contact.template")
                 ;; "* %^{姓名} %^{手机号}p %^{邮箱}p %^{住址}p %^{微信}p %^{微博}p %^{whatsapp}p\n\n  %?"
                 :empty-lines 1 :kill-buffer t))

  (add-to-list 'org-capture-templates
               '("xp"
                 "Passwords"
                 entry
                 (file "~/Dropbox/org/passwords.org.cpt")
                 "* %U - %^{title} %^G\n\n  - 用户名: %^{用户名}\n  - 密码: %(get-or-create-password)"
                 :empty-lines 1 :kill-buffer t))

  (add-to-list 'org-capture-templates
               `("xx"
                 "Blog"
                 plain
                 (file ,(concat "~/Dropbox/org/blog/" (format-time-string "%Y-%m-%d.org")))
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
  (add-to-list 'org-capture-templates
      '("l"
         "Temp Links from the interwebs"
         entry
         (file+headline "~/Dropbox/org/links.org" "Bookmarks")
         "%?\nEntered on %U\n \%i\n %a"
         :immediate-finish t
         :kill-buffer nil))

  (add-to-list 'org-capture-templates
               '("a"
                 "Protocol Annotation"
                 plain
                 (file+function "~/Dropbox/org/links.org" org-capture-template-goto-link)
                 " %^{Title}\n  %U - %?\n\n  %:initial"
                 :empty-lines 1))

  ;; Task Group
  (add-to-list 'org-capture-templates '("t" "Tasks"))

  (add-to-list 'org-capture-templates
               '("ts"                                              ; hotkey
                 "Son Daniel's Task"                               ; title
                 entry                                             ; type
                 (file+headline "~/Dropbox/org/daniel.org" "Task") ; target
                 "** TODO %^{任务}\n%u\n%a\n"                      ; template
                 :clock-in t
                 :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tl"
                 "Wife Lulu's Task"
                 entry
                 (file+headline "~/Dropbox/org/lulu.org" "Task")
                 "** TODO %^{任务}\n%u\n%a\n"
                 :clock-in t
                 :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tr"
                 "My Book Reading Task"
                 entry
                 (file+headline "~/Dropbox/org/xingwenju.org" "Reading")
                 "** TODO %^{书名}\n%u\n%a\n"
                 :clock-in t
                 :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tp"
                 "My Work Projects"
                 entry
                 (file "~/Dropbox/org/projects.org")
                 (file "~/Dropbox/org/capture-template/project.template")
                 :empty-line 1
                 :clock-resume t))
  (add-to-list 'org-capture-templates
               '("tw"
                 "My Work Task"
                 entry
                 (file+headline "~/Dropbox/org/works.org" "Work")
                 "** TODO %^{任务}\n%u\n%a\n"
                 :clock-in t :clock-resume t))

  ;; Most often used
  (add-to-list 'org-capture-templates
               '("P"
                 "My Phone calls"
                 entry
                 (file+headline "~/Dropbox/org/inbox.org" "Phone Calls")
                 (file "~/Dropbox/org/capture-template/phone.template")
                 ;; "* %^{Habit cards|music|balls|games}\n  %?"
                 :new-line 1))

  (add-to-list 'org-capture-templates
               '("h"
                 "My Habit"
                 entry
                 (file "~/Dropbox/org/habit.org")
                 (file "~/Dropbox/org/capture-template/habit.template")
                 ;; "* %^{Habit cards|music|balls|games}\n  %?"
                 :new-line 1))

  (add-to-list 'org-capture-templates
               '("n"
                 "My Notes"
                 entry
                 (file "~/Dropbox/org/notes.org")
                 (file "~/Dropbox/org/capture-template/notes.template")
                 ;; "* %^{Loggings For...} %t %^g\n  %?"
                 :new-line 1))

  (add-to-list 'org-capture-templates
               '("i"
                 "My GTD Inbox"
                 entry
                 (file "~/Dropbox/org/inbox.org")
                 (file "~/Dropbox/org/capture-template/inbox.template")
                 ;; "* [#%^{Priority}] %^{Title} %^g\n SCHEDULED:%U %?\n"
                 :new-line 1))

  ;; template list
  (setq org-structure-template-alist
        (quote (("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>")
                ("e" "#+begin_example\n?\n#+end_example" "<example>\n?\n</example>")
                ("q" "#+begin_quote\n?\n#+end_quote" "<quote>\n?\n</quote>")
                ("v" "#+begin_verse\n?\n#+end_verse" "<verse>\n?\n</verse>")
                ("c" "#+begin_center\n?\n#+end_center" "<center>\n?\n</center>")
                ("l" "#+begin_latex\n?\n#+end_latex" "<literal style=\"latex\">\n?\n</literal>")
                ("L" "#+latex: " "<literal style=\"latex\">?</literal>")
                ("h" "#+begin_html\n?\n#+end_html" "<literal style=\"html\">\n?\n</literal>")
                ("H" "#+html: " "<literal style=\"html\">?</literal>")
                ("a" "#+begin_ascii\n?\n#+end_ascii")
                ("A" "#+ascii: ")
                ("i" "#+index: ?" "#+index: ?")
                ("I" "#+include %file ?" "<include file=%file markup=\"?\">"))))

  (setq org-list-demote-modify-bullet (quote (("+" . "-")
                                              ("*" . "-")
                                              ("1." . "-")
                                              ("1)" . "-")
                                              ("A)" . "-")
                                              ("B)" . "-")
                                              ("a)" . "-")
                                              ("b)" . "-")
                                              ("A." . "-")
                                              ("B." . "-")
                                              ("a." . "-")
                                              ("b." . "-"))))

  (setq org-fast-tag-selection-single-key (quote expert))
  (setq org-tag-alist (quote ((:startgroup)
                              ("@office" . ?o)
                              ("@home" . ?h)
                              ("@travel" . ?t)
                              ("@errand" . ?e)
                              (:endgroup)
                              ("PERSONAL" . ?p)
                              ("KIDS" . ?k)
                              ("DANIEL" . ?d)
                              ("LULU" . ?l)
                              ("WORK" . ?W)
                              ("PROJECT" . ?P)
                              ("COMPUTER" . ?C)
                              ("PHONE" . ?E)
                              ("HABIT" . ?H)
                              )))
  )
