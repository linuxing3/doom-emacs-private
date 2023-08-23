;;; lang/org-private/+capture.el -*- lexical-binding: t; -*-


(add-hook 'org-load-hook #'+org-private|init-capture)

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(setq +org-default-notes-file "gtd.org")
(setq +org-default-todo-file "todo.org")

(setq
 org-default-projects-file (expand-file-name "projects.org" org-directory)
 org-default-works-file (expand-file-name "works.org" org-directory)
 org-default-private-file (expand-file-name "xingwenju.org" org-directory)
 org-default-kid-file (expand-file-name "daniel.org" org-directory)
 org-default-snippets-file (expand-file-name "snippets.org" org-directory)
 org-default-links-file (expand-file-name "links.org" org-directory)
 org-default-habit-file (expand-file-name "habit.org" org-directory)
 org-default-review-file (expand-file-name "review.org" org-directory)
 org-default-journal-file (expand-file-name "journal.org" org-directory)
 ledger-journal-file (expand-file-name "ledger.gpg" org-directory)
 )


(setq org-capture-templates
      `(
        ("t" "Todo" entry
         (file org-default-notes-file)
         "* TODO %^{Logging for...}
:SCHEDULED: %U
:PROPERTIES:
:agenda-group: todo
:END:
%i
%?" )
        ("tl" "Todo with link" entry
         (file org-default-notes-file)
         "* TODO %^{Logging for...}
:SCHEDULED: %U
:PROPERTIES:
:agenda-group: link
:Linked: %a
:END:
%i
%?" )
        ("p" "Project" entry
         (file+headline org-default-projects-file "Projects")
         "** TODO [#B] %? [/]\t%^g\n
:SCHEDULED: %U
:PROPERTIES:
:agenda-group: project
:Linked: %a
:END:
- [ ] Protocolo\n
- [ ] Paper\n
- [ ] Press\n
- [ ] Logistic\n
- [ ] Misc\n"
         :empty-lines 1)
        ;; ==================================
        ("s" "Code Snippet" entry
         (file org-default-snippets-file)
         "* %?\t%^g\n
#+BEGIN_SRC %^{language}\n\n#+END_SRC"
         :empty-lines 1)

        ("wt" "Works Todo" entry
         (file org-default-works-file)
         "* TODO %^{Logging for...}
:SCHEDULED: %U
:PROPERTIES:
:Created: %U
:agenda-group: work
:END:
%i
%?" )
        ("h" "Habit" entry
         (file org-default-habit-file)
         "* %^{Habit for...}
SCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+5d/7d>>\")
:PROPERTIES:
:agenda-group: habit
:STYLE: habit
:END:
%i
%?")
        )
      +org-capture-window-params
      `((name . "org-capture")
        (fullscreen . fullwidth)
        (height . 40)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (left-fringe . 0)
        (right-fringe . 0)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (undecorated . t)
        (no-special-glyphs . t)
        (ns-appearance . nil)
        (window-system . ,(cond (IS-MAC 'ns)
                                (IS-LINUX 'x)
                                (t 'w32)))
        ,(if IS-LINUX '(display . ":0")))
      )
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; template list
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(setq org-tag-alist (quote ((:startgroup)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@travel" . ?t)
                            ("@errand" . ?e)
                            (:endgroup)
                            ("PERSONAL" . ?p)
                            ("WORK" . ?w)
                            ("PROJECT" . ?P)
                            )))

(defun +org-private|init-capture ()
  (add-hook 'org-capture-prepare-finalize-hook #'counsel-org-tag)
  )
