;;; ~/.doom.d/autoload/+xingwenju.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +xingwenju/find-in-dotfiles ()
  "Open a file somewhere in ~/dotfiles via a fuzzy filename search."
  (interactive)
  (doom-project-find-file (expand-file-name "~/dotfiles")))

;;;###autoload
(defun +xingwenju/browse-dotfiles ()
  "Browse the files in ~/dotfiles."
  (interactive)
  (doom-project-browse (expand-file-name "~/dotfiles")))

;;;###autoload
(defun +xingwenju/find-notes-for-major-mode ()
  "Open a file in ~/Dropbox/org/code"
  (interactive)
  (let ((default-directory (expand-file-name "code/" +org-dir)))
    (doom-project-browse default-directory)))

;;;###autoload
(defun +xingwenju/find-notes-for-project ()
  "Open a file in ~/Dropbox/org/projects"
  (interactive)
  (let ((default-directory (expand-file-name "projects/" +org-dir)))
    (doom-project-browse default-directory)))
