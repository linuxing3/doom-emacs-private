;;; extensions/org+pretty.el -*- lexical-binding: t; -*-


;; Change display with styles
(after! org
  (set-face-attribute 'org-link nil
                      :weight 'normal
                      :background nil)
  (set-face-attribute 'org-code nil
                      :foreground "#a9a1e1"
                      :background nil)
  (set-face-attribute 'org-date nil
                      :foreground "#5B6268"
                      :background nil)
  (set-face-attribute 'org-level-1 nil
                      :foreground "steelblue2"
                      :background nil
                      :height 1.1
                      :weight 'normal)
  (set-face-attribute 'org-level-2 nil
                      :foreground "slategray2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-3 nil
                      :foreground "SkyBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-4 nil
                      :foreground "DodgerBlue2"
                      :background nil
                      :height 1.0
                      :weight 'normal)
  (set-face-attribute 'org-level-5 nil
                      :weight 'normal)
  (set-face-attribute 'org-level-6 nil
                      :weight 'normal)
  (set-face-attribute 'org-document-title nil
                      :foreground "SlateGray1"
                      :background nil
                      :height 1.25
                      :weight 'bold))
(after! org

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
                              ))))
