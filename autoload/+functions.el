;;; +functions.el -*- lexical-binding: t; -*-
;;;###autoload
(defun +private/duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;;;###autoload
(defun +private/duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))


;; from @bmag spacemacs
;;;###autoload
(defun +private/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((window-tree (car (window-tree)))
           (current-split-vertical-p (car window-tree))
           (first-window (nth 2 window-tree))
           (second-window (nth 3 window-tree))
           (second-window-state (window-state-get second-window))
           (splitter (if current-split-vertical-p
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows first-window)
      ;; `window-state-put' also re-selects the window if needed, so we don't
      ;; need to call `select-window'
      (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;###autoload
(defun +private/swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

;;;###autoload
(defun +private/swiper-all-region-or-symbol ()
  "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper-all input)))

;;;###autoload
(defun +private/counsel-region-or-symbol ()
  "Run `counsel-ag' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (counsel-ag input)))

;; from http://stackoverflow.com/questions/9656311/conflict-resolution-with-emacs-ediff-how-can-i-take-the-changes-of-both-version
;;;###autoload
(defun +private/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

;;;###autoload
;; Dos2Unix/Unix2Dos
(defun +private/dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun +private/unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))


;;;###autoload
(defun +private/insert-key-then-command (key)
  "Ask for a key then insert its description.
Will work on both org-mode and any mode that accepts plain html."
  (interactive "kType key sequence: ")
  (let* ((orgp (derived-mode-p 'org-mode))
         (tag (if orgp "~%s~" "<kbd>%s</kbd>")))
    (insert (format tag (help-key-description key nil)))
    (kill-new  (symbol-name (cadr (help--analyze-key key nil))))
    (forward-char (if orgp -1 -6))))


;;;###autoload
(defun +private/find-url (url)
  "Download URL and insert into current buffer at point."
  (interactive "sULR: ")
  (insert (my-url-retrieve url)))

(defun +private/my-url-retrieve (url &optional no-headers)
  "Retrieve and return as string the content of URL.

If NO-HEADERS is non-nil, remove the HTTP headers first."
  (with-current-buffer (url-retrieve-synchronously url)
    (when no-headers
      (goto-char (point-min))
      (search-forward "\n\n")
      (delete-region (point-min) (point)))
    (prog1 (buffer-string)
      (kill-buffer))))

;;;###autoload
(defun +private/insert-date-iso (date)
  "Insert timestamp."
  (interactive (list (org-read-date)))
  (insert date))

;;;###autoload
(defun +private/md5-file (filename)
  "Open FILENAME, load it into a buffer and generate the md5 of its contents"
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents filename)
    (let ((hash (md5 (current-buffer))))
      (when (called-interactively-p 'interactive)
        (message hash))
      hash)))


;;;###autoload
(defun +private/create-scratch-buffer-current-mode ()
  "Create a new scratch buffer to work in and set its mode to current `major-mode'."
  (interactive)
  (create-scratch-buffer major-mode))

;;;###autoload
(defun +private/save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (name-last-kbd-macro name)            ; use this name for the macro
     (find-file user-init-file)            ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer


;;;###autoload
(defun +private/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: "
                                      (file-name-directory filename)
                                      (file-name-nondirectory filename)
                                      nil
                                      (file-name-nondirectory filename))))
        (cond ((get-buffer new-name)
               (error "A buffer named '%s' already exists!" new-name))
              (t
               (rename-file filename new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil)
               (message "File '%s' successfully renamed to '%s'" name (file-name-nondirectory new-name))))))))

;;;###autoload
(defun +private/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;###autoload
(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  ;;(let ((m-m major-mode))
  ;;(fundamental-mode)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8)
  ;;(funcall m-m)
  ;;)
  )

;;;###autoload
(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (cleanup-buffer-safe)
  (indent-buffer))

;;;###autoload
(defun toggle-mode-line-to-header ()
  "toggles the modeline to header"
  (interactive)
  (setq mode-line-format
        (if (equal mode-line-format nil)
            (progn
              (setq header-line-format (default-value 'header-line-format))
              (default-value 'mode-line-format))
          (let ((hlf header-line-format))
            (setq header-line-format mode-line-format)
            hlf)))
  (redraw-display))


(defun +private/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state
                                                     "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state
                                                     "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state
                                                     "init caps") )
         (t (put this-command 'state "all lower") ))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))


(defun +private/replace-next-underscore-with-camel (arg)
  (interactive "p")
  (if (> arg 0)
 (setq arg (1+ arg))) ; 1-based index to get eternal loop with 0
  (let ((case-fold-search nil))
    (while (not (= arg 1))
      (search-forward-regexp "\\b_[a-z]")
      (forward-char -2)
      (delete-char 1)
      (capitalize-word 1)
      (setq arg (1- arg)))))


;;;###autoload
(defun +private/goto-main-window (pname frame)
    (let ((window (car (+my-doom-visible-windows))))
      (if (window-live-p window)
          (select-window window))))

;;;###autoload
(defun +private/show-visible-windows (&optional window-list)
  "Return a list of the visible, non-popup windows."
  (cl-loop for window in (or window-list (window-list))
           unless (window-dedicated-p window)
           collect window))
