;;; org+config.el -*- lexical-binding: t; -*-
;;;
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;; Hooks
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'prettify-symbols-mode)

(after! org
  ;; Add built-in modules of org
  (setq
   org-modules (quote (org-bibtex org-habit org-protocol org-mac-link))
   org-ellipsis " ▼ "
   org-bullets-bullet-list '(" ○" " ◆")
   org-tags-column -80
   )

  (setq
   ;; org-fancy-priorities-list '("[A]" "[B]" "[C]")
   ;; org-fancy-priorities-list '("❗" "[B]" "[C]")
   org-fancy-priorities-list '("🟥" "🟧" "🟨")
   org-priority-faces
   '((?A :foreground "#ff6c6b" :weight bold)
     (?B :foreground "#98be65" :weight bold)
     (?C :foreground "#c678dd" :weight bold))
   org-agenda-block-separator 8411)

  (defun dt/org-colors-doom-one ()
    "Enable Doom One colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#51afef" ultra-bold)
           (org-level-2 1.6 "#c678dd" extra-bold)
           (org-level-3 1.5 "#98be65" bold)
           (org-level-4 1.4 "#da8548" semi-bold)
           (org-level-5 1.3 "#5699af" normal)
           (org-level-6 1.2 "#a9a1e1" normal)
           (org-level-7 1.1 "#46d9ff" normal)
           (org-level-8 1.0 "#ff6c6b" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-dracula ()
    "Enable Dracula colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#8be9fd" ultra-bold)
           (org-level-2 1.6 "#bd93f9" extra-bold)
           (org-level-3 1.5 "#50fa7b" bold)
           (org-level-4 1.4 "#ff79c6" semi-bold)
           (org-level-5 1.3 "#9aedfe" normal)
           (org-level-6 1.2 "#caa9fa" normal)
           (org-level-7 1.1 "#5af78e" normal)
           (org-level-8 1.0 "#ff92d0" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-gruvbox-dark ()
    "Enable Gruvbox Dark colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#458588" ultra-bold)
           (org-level-2 1.6 "#b16286" extra-bold)
           (org-level-3 1.5 "#98971a" bold)
           (org-level-4 1.4 "#fb4934" semi-bold)
           (org-level-5 1.3 "#83a598" normal)
           (org-level-6 1.2 "#d3869b" normal)
           (org-level-7 1.1 "#d79921" normal)
           (org-level-8 1.0 "#8ec07c" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-monokai-pro ()
    "Enable Monokai Pro colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#78dce8" ultra-bold)
           (org-level-2 1.6 "#ab9df2" extra-bold)
           (org-level-3 1.5 "#a9dc76" bold)
           (org-level-4 1.4 "#fc9867" semi-bold)
           (org-level-5 1.3 "#ff6188" normal)
           (org-level-6 1.2 "#ffd866" normal)
           (org-level-7 1.1 "#78dce8" normal)
           (org-level-8 1.0 "#ab9df2" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-nord ()
    "Enable Nord colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#81a1c1" ultra-bold)
           (org-level-2 1.6 "#b48ead" extra-bold)
           (org-level-3 1.5 "#a3be8c" bold)
           (org-level-4 1.4 "#ebcb8b" semi-bold)
           (org-level-5 1.3 "#bf616a" normal)
           (org-level-6 1.2 "#88c0d0" normal)
           (org-level-7 1.1 "#81a1c1" normal)
           (org-level-8 1.0 "#b48ead" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-oceanic-next ()
    "Enable Oceanic Next colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#6699cc" ultra-bold)
           (org-level-2 1.6 "#c594c5" extra-bold)
           (org-level-3 1.5 "#99c794" bold)
           (org-level-4 1.4 "#fac863" semi-bold)
           (org-level-5 1.3 "#5fb3b3" normal)
           (org-level-6 1.2 "#ec5f67" normal)
           (org-level-7 1.1 "#6699cc" normal)
           (org-level-8 1.0 "#c594c5" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-palenight ()
    "Enable Palenight colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#82aaff" ultra-bold)
           (org-level-2 1.6 "#c792ea" extra-bold)
           (org-level-3 1.5 "#c3e88d" bold)
           (org-level-4 1.4 "#ffcb6b" semi-bold)
           (org-level-5 1.3 "#a3f7ff" normal)
           (org-level-6 1.2 "#e1acff" normal)
           (org-level-7 1.1 "#f07178" normal)
           (org-level-8 1.0 "#ddffa7" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-solarized-dark ()
    "Enable Solarized Dark colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#268bd2" ultra-bold)
           (org-level-2 1.6 "#d33682" extra-bold)
           (org-level-3 1.5 "#859900" bold)
           (org-level-4 1.4 "#b58900" semi-bold)
           (org-level-5 1.3 "#cb4b16" normal)
           (org-level-6 1.2 "#6c71c4" normal)
           (org-level-7 1.1 "#2aa198" normal)
           (org-level-8 1.0 "#657b83" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-solarized-light ()
    "Enable Solarized Light colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#268bd2" ultra-bold)
           (org-level-2 1.6 "#d33682" extra-bold)
           (org-level-3 1.5 "#859900" bold)
           (org-level-4 1.4 "#b58900" semi-bold)
           (org-level-5 1.3 "#cb4b16" normal)
           (org-level-6 1.2 "#6c71c4" normal)
           (org-level-7 1.1 "#2aa198" normal)
           (org-level-8 1.0 "#657b83" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  (defun dt/org-colors-tomorrow-night ()
    "Enable Tomorrow Night colors for Org headers."
    (interactive)
    (dolist
        (face
         '((org-level-1 1.7 "#81a2be" ultra-bold)
           (org-level-2 1.6 "#b294bb" extra-bold)
           (org-level-3 1.5 "#b5bd68" bold)
           (org-level-4 1.4 "#e6c547" semi-bold)
           (org-level-5 1.3 "#cc6666" normal)
           (org-level-6 1.2 "#70c0ba" normal)
           (org-level-7 1.1 "#b77ee0" normal)
           (org-level-8 1.0 "#9ec400" normal)))
      (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
    (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf"))

  ;; Load our desired dt/org-colors-* theme on startup
  (dt/org-colors-doom-one)

  ;; Some directories' location
  (setq diary-file (dropbox-path "org/diary"))
  ;; Add all files under org-directory folder to agenda
  (setq
   org-agenda-diary-file (dropbox-path "org/diary")
   org-agenda-files (directory-files org-directory t "\\.agenda\\.org$" t))

  ;; Customize archived location
  (setq org-archive-location (dropbox-path "org/archived/%s_archive::"))

  ;; RSS with Elfeed
  (setq rmh-elfeed-org-files (list
                              (concat org-directory "/elfeed1.org")
                              (concat org-directory "/elfeed2.org")))
  (setq elfeed-db-directory (concat org-directory "/elfeed/db/"))
  (setq elfeed-enclosure-default-dir (concat org-directory "/elfeed/enclosures/"))
  (setq elfeed-search-filter "@3-month-ago +unread")

  ;; Stuck projects
  (setq org-stuck-projects
        '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

  ;; Config refile targets
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-targets
        '((nil :maxlevel . 4)
          (org-agenda-files :maxlevel . 4)))

  ;; Column View
  (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))))

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


(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

