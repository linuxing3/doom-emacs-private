;;; autoload/editor.el -*- lexical-binding: t; -*-

;;; My Own Awesome™ Functions

(require 'dash)
(require 'dash-functional)
(require 's)

;; Scroll and recenter

(defun +private/escape-and-save-buffer ()
  "Escape to evil normal state and save file"
  (interactive)
  (if (evil-insert-state-p)
    (progn
      (evil-force-normal-state)
      (save-buffer))
    (save-buffer)))

;;;###autoload
(defun +private/up-scroll (n)
  "Scroll up marker and line N times."
  (interactive "p")
  (line-move (* -1 n))
  (unless (eq (window-start) (point-min))
    (scroll-down n)))

;;;###autoload
(defun +private/down-scroll (n)
  "Scroll down marker and line N times."
  (interactive "p")
  (line-move n)
  (unless (eq (window-end) (point-max))
    (scroll-up n)))

;; Delete a word forward without pasting in the kill-region
;;;###autoload
(defun +private/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

;; Delete a word backwards without modifying the kill-region
;;;###autoload
(defun +private/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (+private/delete-word (- arg)))

;;;###autoload
(defun +private/kill-line ()
  "Like kill-line but without adding anything to the kill ring."
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (if kill-whole-line (delete-char 1)))

;;;###autoload
; Functions to easily toggle the recording of macros.
(defun +private/macro-on ()
  "One-key keyboard macros: turn recording on."
  (interactive)
  (define-key global-map (this-command-keys)
    '+private/macro-off)
  (start-kbd-macro nil))

;;;###autoload
(defun +private/macro-off ()
  "One-key keyboard macros: turn recording off."
  (interactive)
  (define-key global-map (this-command-keys)
    '+private/macro-on)
  (end-kbd-macro))

;;;###autoload
(defun +private/scrap-devto-to-current-buffer ()
    "Scrap a webpage to current buffer"
    (interactive)
    (require 'elquery)
    (with-current-buffer (get-buffer-create "*dev*")
      (--each (with-current-buffer (url-retrieve-synchronously "https://dev.to")
                (prog1 (->> (buffer-string)
                            elquery-read-string
                            (elquery-$ "div.single-article h3")
                            (mapcar #'elquery-text))
                  (kill-buffer)))
        (insert it "\n"))))
