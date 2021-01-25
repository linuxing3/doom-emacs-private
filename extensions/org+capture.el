;;; org+capture.el -*- lexical-binding: t; -*-

(after! org

  ;; Capture template

  (setq org-capture-templates nil)

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
