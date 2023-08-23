;;; lang/org-private/autoload/org.el -*- lexical-binding: t; -*-

(require 'cl)

;;;###autoload
(defun gtd ()
  (interactive)
  (find-file "~/Dropbox/org/inbox.org"))

;;;###autoload
(defun tutorial ()
  (interactive)
  (find-file "~/Dropbox/org/tutorial.org"))

;;;###autoload
(defun links ()
  (interactive)
  (find-file "~/Dropbox/org/links.org"))

;;;###autoload
(defun work ()
  (interactive)
  (find-file "~/Dropbox/org/work.org"))

;;;###autoload
(defun me ()
  (interactive)
  (find-file "~/Dropbox/org/xingwenju.org"))

;;;###autoload
(defun kid ()
  (interactive)
  (find-file "~/Dropbox/org/daniel.org"))

;;;###autoload
(defun snippet ()
  (interactive)
  (find-file "~/Dropbox/org/snippets.org"))

;;;###autoload
(defun notes ()
  (interactive)
  (find-file "~/Dropbox/org/notes.org"))

;;;###autoload
(defun journal ()
  (interactive)
  (find-file "~/Dropbox/org/journal"))

;;;###autoload
(defun export-diary-from-cal ()
  (interactive)
  (start-process-shell-command "export diary" nil "/Users/linuxing3/.emacs.d-backup/private/local/calendardiary 30 > /Users/linuxing3/Dropbox/org/cal.diary"))

;;;###autoload
(defun +org/open-brain-here ()
  (interactive)
  (let ((org-brain-path (projectile-project-root)))
    (call-interactively 'org-brain-visualize)))

;;;###autoload
(defun reflash-indentation ()
  "Fix org-indent issues, center line."
  (interactive)
  (org-indent-mode 1)
  (recenter-top-bottom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Journal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-insert-plain-text-life-org-template ()
  "This document is available as an [[http://doc.norang.ca/org-mode.org] [org file]]
   which you can load in Emacs and tangle with =C-c C-v C-t= which will create org-mode.el
   in the same directory as the org-mode.org file.  This will extract all of the elisp examples
   in this document into a file you can include in your .emacs file."
  (interactive)
  (load-file "~/Dropbox/org/inbox.org")
  )

;;;###autoload
(defun get-journal-file-today ()
  "Return filename for today's journal entry."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir "/" daily-name ".org"))))

;;;###autoload
(defun get-journal-text-file-today ()
  "Return filename for today's journal entry with txt."
  (let ((daily-name (format-time-string "%Y%m%d")))
    (expand-file-name (concat org-journal-dir "/" daily-name ".txt"))))

;;;###autoload
(defun my-journal-file-today ()
  "Create and load a journal file based on today's date. With org extension"
  (interactive)
  (find-file (get-journal-file-today)))

;;;###autoload
(defun my-journal-text-file-today ()
  "Create and load a journal file based on today's date. File with extension"
  (interactive)
  (find-file (get-journal-text-file-today)))

;;;###autoload
(defun get-journal-file-yesterday ()
  "Return filename for yesterday's journal entry."
  (interactive)
  (let* ((yesterday (time-subtract (current-time) (days-to-time 1)))
         (daily-name (format-time-string "%Y%m%d" yesterday)))
    (expand-file-name (concat org-journal-dir "/" daily-name ".org"))))

;;;###autoload
(defun my-journal-file-yesterday ()
  "Creates and load a file based on yesterday's date."
  (interactive)
  (find-file (get-journal-file-yesterday)))

;;;###autoload
(defun my-journal-insert-title ()
  "Insert's the journal heading based on the file's name."
  (interactive)
  (save-excursion
    (let* (datim (format-time-string "%Y-%d-%m"))
      (setq title (read-from-minibuffer "Title: " ""))
      (setq author (read-from-minibuffer "Author: " ""))
      (insert "#+begin_src yaml :exports results :results value html\n")
      (insert "---\n")
      (insert (format "title: %s\n" title))
      (insert (format "author: %s\n" author))
      (insert (format "excerpt: %s\n" title))
      (insert (format "date: %s\n" (format-time-string "%Y-%m-%d")))
      (insert "---\n")
      (insert "#+end_src\n\n\n")
      (insert "* Heading one\n\n\n")
      (insert "* Heading two\n\n\n")
      (insert "* Reference\n\n\n")
      (insert (format "#+title: %s\n" title))
      (insert (format "#+author: %s\n" author))
      (insert (format "#+excerpt: %s\n" title))
      (insert (format "#+date: %s\n" (format-time-string "%Y-%m-%d"))))))

;;;###autoload
(defun journal-last-year-file ()
  "Returns the string corresponding to the journal entry that
    happened 'last year' at this same time (meaning on the same day
    of the week)."
  (let* ((last-year-seconds (- (float-time) (* 365 24 60 60)))
         (last-year (seconds-to-time last-year-seconds))
         (last-year-dow (nth 6 (decode-time last-year)))
         (this-year-dow (nth 6 (decode-time)))
         (difference (if (> this-year-dow last-year-dow)
                         (- this-year-dow last-year-dow)
                       (- last-year-dow this-year-dow)))
         (target-date-seconds (+ last-year-seconds (* difference 24 60 60)))
         (target-date (seconds-to-time target-date-seconds)))
    (format-time-string "%Y%m%d" target-date)))

;;;###autoload
(defun my-journal-last-year ()
  "Loads last year's journal entry, which is not necessary the
    same day of the month, but will be the same day of the week."
  (interactive)
  (let ((journal-file (concat org-journal-dir "/" (journal-last-year-file))))
    (find-file journal-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Meeting Notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-meeting-notes ()
  "Call this after creating an org-mode heading for where the notes for the meeting
     should be. After calling this function, call 'meeting-done' to reset the environment."
  (interactive)
  (outline-mark-subtree)                              ;; Select org-mode section
  (narrow-to-region (region-beginning) (region-end))  ;; Only show that region
  (deactivate-mark)
  (delete-other-windows)                              ;; Get rid of other windows
  (text-scale-set 2)                                  ;; Text is now readable by others
  (fringe-mode 0)
  (message "When finished taking your notes, run meeting-done."))

;;;###autoload
(defun my-meeting-done ()
  "Attempt to 'undo' the effects of taking meeting notes."
  (interactive)
  (widen)                                       ;; Opposite of narrow-to-region
  (text-scale-set 0)                            ;; Reset the font size increase
  (fringe-mode 1)
  (winner-undo))                                ;; Put the windows back in place

;;;###autoload
(defun my-prepare-meeting-notes ()
  "Prepare meeting notes for email
   Take selected region and convert tabs to spaces, mark TODOs with leading >>>, and copy to kill ring for pasting"
  (interactive)
  (let (prefix)
    (save-excursion
      (save-restriction
        (narrow-to-region (region-beginning) (region-end))
        (untabify (point-min) (point-max))
        (goto-char (point-min))
        (while (re-search-forward "^\\( *-\\\) \\(TODO\\|DONE\\): " (point-max) t)
          (replace-match (concat (make-string (length (match-string 1)) ?>) " " (match-string 2) ": ")))
        (goto-char (point-min))
        (kill-ring-save (point-min) (point-max))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Screenshot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-insert-org-or-md-img-link (prefix imagename)
  (if (equal (file-name-extension (buffer-file-name)) "org")
      (insert (format "[[%s%s]]" prefix imagename))
    (insert (format "![%s](%s%s)" imagename prefix imagename))))

;;;###autoload
(defun my-capture-screenshot (basename)
  "Take a screenshot into a time stamped unique-named file in the
  same directory as the org-buffer/markdown-buffer and insert a link to this file."
  (interactive "sScreenshot name: ")
  (if (equal basename "")
      (setq basename (format-time-string "%Y%m%d_%H%M%S")))
  (setq fullpath
        (concat (file-name-directory (buffer-file-name))
                "../attach/"
                (file-name-base (buffer-file-name))
                "_"
                basename))
  (setq relativepath
        (concat (file-name-base (buffer-file-name))
                "_"
                basename
                ".png"))
  (if (file-exists-p (file-name-directory fullpath))
      (progn
        (setq final-image-full-path (concat fullpath ".png"))
        (call-process "screencapture" nil nil nil "-s" final-image-full-path)
        (if (executable-find "convert")
            (progn
              (setq resize-command-str (format "convert %s -resize 800x600 %s" final-image-full-path final-image-full-path))
              (shell-command-to-string resize-command-str)))
        (my-insert-org-or-md-img-link "../attac![split window in emacs.png](./split window in emacs.png)
h/" relativepath))
    (progn
      (call-process "screencapture" nil nil nil "-s" (concat basename ".png"))
      (my-insert-org-or-md-img-link "./" (concat basename ".png"))))
  (insert "\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction
        (org-narrow-to-subtree)
        (org-show-subtree)
        (goto-char (point-min))
        (beginning-of-line 2)
        (narrow-to-region (point) (point-max))
        (org-map-entries
         '(when (member (org-get-todo-state) org-done-keywords)
            (org-todo (car org-todo-keywords))))))))

;;;###autoload
(defun my-org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

;;;###autoload
(defun my-org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

;;;###autoload
(defun my-org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'file))

;;;###autoload
(defun my-org-archive-cancel-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/CANCELLED" 'file))

;; "https://github.com/vhallac/.emacs.d/blob/master/config/customize-org-agenda.el"
;;;###autoload
(defun my-skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      ;; VH: I changed this line from
      ;; (if (bh/is-project-p)
      (if (and (eq (point) (bh/find-project-task))
               (bh/is-project-p))
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

;;;###autoload
(defun my-load-org-tasks ()
  (interactive)
  (shell-command (format "/usr/local/bin/michel-orgmode --pull --orgfile %s" org-default-tasks-file))
  (find-file org-default-tasks-file)
  (ha/first-header)
  (add-hook 'after-save-hook 'ha/save-org-tasks t t))

;;;###autoload
(defun my-save-org-tasks ()
  (save-buffer)
  (shell-command (format "/usr/local/bin/michel-orgmode --push --orgfile %s" org-default-tasks-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Source block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-complete-org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

;;;###autoload
(defun my-simple-org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "sh" "js")))
     (list (ido-completing-read "代码类型: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)    ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;;;###autoload
(defun my-filter-by-tags ()
  (let ((head-tags (org-get-tags-at)))
    (member current-tag head-tags)))

;;; Clocks

;;;###autoload
(defun my-org-clock-sum-today-by-tags (timerange &optional tstart tend noinsert)
  (interactive "P")
  (let* ((timerange-numeric-value (prefix-numeric-value timerange))
         (files (org-add-archive-files (org-agenda-files)))
         (include-tags '("WORK" "EMACS" "DREAM" "WRITING" "MEETING"
                         "LIFE" "PROJECT" "OTHER"))
         (tags-time-alist (mapcar (lambda (tag) `(,tag . 0)) include-tags))
         (output-string "")
         (tstart (or tstart
                     (and timerange (equal timerange-numeric-value 4) (- (org-time-today) 86400))
                     (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "Start Date/Time:"))
                     (org-time-today)))
         (tend (or tend
                   (and timerange (equal timerange-numeric-value 16) (org-read-date nil nil nil "End Date/Time:"))
                   (+ tstart 86400)))
         h m file item prompt donesomething)
    (while (setq file (pop files))
      (setq org-agenda-buffer (if (file-exists-p file)
                                  (org-get-agenda-file-buffer file)
                                (error "No such file %s" file)))
      (with-current-buffer org-agenda-buffer
        (dolist (current-tag include-tags)
          (org-clock-sum tstart tend 'my-filter-by-tags)
          (setcdr (assoc current-tag tags-time-alist)
                  (+ org-clock-file-total-minutes (cdr (assoc current-tag tags-time-alist)))))))
    (while (setq item (pop tags-time-alist))
      (unless (equal (cdr item) 0)
        (setq donesomething t)
        (setq h (/ (cdr item) 60)
              m (- (cdr item) (* 60 h)))
        (setq output-string (concat output-string (format "[-%s-] %.2d:%.2d\n" (car item) h m)))))
    (unless donesomething
      (setq output-string (concat output-string "[-Nothing-] Done nothing!!!\n")))
    (unless noinsert
      (insert output-string))
    output-string))

;;;###autoload
(defun my-first-header ()
  (goto-char (point-min))
  (search-forward-regexp "^\* ")
  (beginning-of-line 1)
  (point))


;;;###autoload
(defun my-org-text-bold () "Wraps the region with asterisks."
       (interactive)
       (surround-text "*"))

;;;###autoload
(defun my-org-text-italics () "Wraps the region with slashes."
       (interactive)
       (surround-text "/"))

;;;###autoload
(defun my-org-text-code () "Wraps the region with equal signs."
       (interactive)
       (surround-text "="))

;;;###autoload
(defun my-org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
A double return on an empty element deletes it.
Use a prefix arg to get regular RET. "
  (interactive "P")
  (if ignore
      (org-return)
    (cond
     ;; Open links like usual
     ((eq 'link (car (org-element-context)))
      (org-return))
     ;; lists end with two blank lines, so we need to make sure we are also not
     ;; at the beginning of a line to avoid a loop where a new entry gets
     ;; created with only one blank line.
     ((and (org-in-item-p) (not (bolp)))
      (if (org-element-property :contents-begin (org-element-context))
          (org-insert-heading)
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     ((org-at-heading-p)
      (if (not (string= "" (org-element-property :title (org-element-context))))
          (progn (org-end-of-meta-data)
                 (org-insert-heading))
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")))
     ((org-at-table-p)
      (if (-any?
           (lambda (x) (not (string= "" x)))
           (nth
            (- (org-table-current-dline) 1)
            (org-table-to-lisp)))
          (org-return)
        ;; empty row
        (beginning-of-line)
        (setf (buffer-substring
               (line-beginning-position) (line-end-position)) "")
        (org-return)))
     (t
      (org-return)))))

;;;###autoload
(defun my-org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "c")
  (org-agenda-fortnight-view))

;;;###autoload
(defun my-org-headline-to-top ()
  (interactive)
  ;; check if we are at the top level
  (let ((lvl (org-current-level)))
    (cond
     ;; above all headlines so nothing to do
     ((not lvl)
      (message "No headline to move"))
     ((= lvl 1)
      ;; if at top level move current tree to go above first headline
      (org-cut-subtree)
      (beginning-of-buffer)
      ;; test if point is now at the first headline and if not then
      ;; move to the first headline
      (unless (looking-at-p "*")
        (org-next-visible-heading 1))
      (org-paste-subtree))
     ((> lvl 1)
      ;; if not at top level then get position of headline level above
      ;; current section and refile to that position. Inspired by
      ;; https://gist.github.com/alphapapa/2cd1f1fc6accff01fec06946844ef5a5
      (let* ((org-reverse-note-order t)
             (pos (save-excursion
                    (outline-up-heading 1)
                    (point)))
             (filename (buffer-file-name))
             (rfloc (list nil filename nil pos)))
        (org-refile nil nil rfloc))))))

;;;###autoload
(defun my-org-agenda-item-to-top ()
  (interactive)
  ;; save buffers to preserve agenda
  (org-save-all-org-buffers)
  ;; switch to buffer for current agenda item
  (org-agenda-switch-to)
  ;; move item to top
  (bjm/org-headline-to-top)
  ;; go back to agenda view
  (switch-to-buffer (other-buffer (current-buffer) 1))
  ;; refresh agenda
  (org-agenda-redo)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; 转el文件为org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-change-el-to-org (from-file-name directory)
  "Recursively change el files to org files for tangle"
  (interactive)
  (let ((to-file-name from-file-name) (default-directory directory))
    (switch-to-buffer-other-window "*temp*")
    (erase-buffer)
    (insert-file-contents from-file-name)
    (goto-char (point-min))
    (insert (concat "* " from-file-name " configurations"))
    (insert "\n\n#+BEGIN_SRC emacs-lisp\n\n")
    (goto-char (point-max))
    (insert "\n#+END_SRC\n")
    (goto-char (point-min))
    (write-file (concat to-file-name ".org"))
    (other-window 1)
    ))


;;;;; Walk a directory

;;;###autoload
(defun my-walk-and-change-el-to-org (directory)
  "Walk a directory and apply function to each file"
  (interactive)
  (mapcar
   (lambda (elm)
     (unless (or (string= elm ".") (string= elm ".."))
       (funcall 'my-change-el-to-org elm directory)))
   (directory-files directory)))

;;;###autoload
(defun my-enhanced-org-face ()
  (progn
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
				         ("^\\*+ \\(DOING\\) "
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
