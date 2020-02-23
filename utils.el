;;; c:/Users/Administrator/.doom.d/utils.el -*- lexical-binding: t; -*-

(defun os-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
    (expand-file-name path "C:/Users/Administrator")
    (expand-file-name path "/home/linuxing3")))

(defun dropbox-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
    (expand-file-name path "D:/Dropbox")
    (expand-file-name path "/home/linuxing3/Dropbox")))

(defun my-development-environment ()
  ;; Ensure the babel load file type
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (emacs-lisp . t)
       (python . t)
       (ein . t)
       (javacript . t)
       (typescript . t)
       (shell . t)
       (R . t)
       (ditaa . t)
       (plantuml . t)
       ))))
