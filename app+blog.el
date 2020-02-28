;;; c:/Users/Administrator/.doom.d/app+blog.el -*- lexical-binding: t; -*-

(require 'cl)

(defvar blog-gridsome-base-dir nil
  "Netlify gridsome base directory")

(defvar blog-gridsome-process "Gridsome Develop"
  "Name of 'gridsome develop' process process")

(setq blog-gridsome-base-dir "D:/workspace/gridsome.org")

(defun my-blog-gridsome-find-dir ()
  "Open gatsby blog files"
  (interactive)
  (find-file "D:/workspace/gridsome.org/blog"))

(defun my-blog-gridsome-deploy ()
  "Run gridsome cli and push changes upstream."
  (interactive)
  (with-dir blog-gridsome-base-dir
			;; deploy to github for ci
            (shell-command "git add .")
            (--> (current-time-string)
               (concat "git commit -m \"" it "\"")
               (shell-command it))
            (shell-command "git push -u origin master")
  ))

(defun my-blog-gridsome-start-server ()
  "Run gridsome server if not already running and open its webpage."
  (interactive)
  (with-dir blog-gatsby-base-dir
            (unless (get-process blog-gridsome-process)
              (start-process blog-gridsome-process nil "gridsome" "develop" "-H" "0.0.0.0"))
			))
(defun my-blog-gridsome-end-server ()
  "End gridsome server process if running."
  (interactive)
  (--when-let (get-process blog-gridsome-process)
    (delete-process it)))

;;;###autoload
 (defun my-find-gridsome-filepath (gridsome-post-subdir gridsome-post-filename)
	"Create the gridsome post in a specific hugo post directory"
	(interactive
		(let ((gridsome-post-subdirs '("blog" "docs" )))
				(list (ido-completing-read "Directory Name:" gridsome-post-subdirs))
		))
	(progn
			(setq daily-name (format-time-string "%Y-%m-%d"))
      (if (string= gridsome-post-subdir "docs")
        ;; filpath: gridsome.org/docs/write-blog.md
        (setq gridsome-post-filepath
          (concat blog-gridsome-base-dir "/" gridsome-post-subdir "/" gridsome-post-filename ".md"))
        ;; filepath: gridsome.org/blog/2018-01-01-write-blog/index.md
        (setq gridsome-post-filepath
          (concat blog-gridsome-base-dir "/" gridsome-post-subdir "/" daily-name "-" gridsome-post-filename "/index.md")))
      (format "%s" gridsome-post-filepath)))

;;;###autoload
 (defun my-gridsome-create-newpost-empty ()
	"Create the gridsome post in a specific hugo post directory"
	(interactive)
	(progn
    (setq gridsome-post-subdir (ido-completing-read "Directory Name: " '("blog" "docs")))
   	 (setq gridsome-post-filepath (my-find-gridsome-filepath gridsome-post-subdir (read-from-minibuffer "File Name: " "write-blog")))
     (find-file gridsome-post-filepath)
     (goto-char (point-min))
     (insert "---\n")
     (insert (concat "title: \n"))
     (insert "author: Xing Wenju\n")
     (insert (concat "date: " daily-name "\n"))
     (insert "excerpt: \n")
     (insert "---\n")))

;;;###autoload
(defun my-org-export-md-to-gridsome-newpost ()
	"Saving the current org file as a gridsome post
in a specific hugo post directory"
	(interactive)
	(progn
    (setq gridsome-post-subdir (ido-completing-read "Directory Name: " '("blog" "docs")))
   	(setq gridsome-post-filepath (my-find-gridsome-filepath gridsome-post-subdir (read-from-minibuffer "File Name: " "write-blog")))
    (message gridsome-post-filepath)
    (org-md-export-as-markdown)
    (evil-save gridsome-post-filepath)
    (evil-window-delete)
    (find-file gridsome-post-filepath)))