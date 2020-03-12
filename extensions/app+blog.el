;;; c:/Users/Administrator/.doom.d/app+blog.el -*- lexical-binding: t; -*-

(require 'cl)

(defvar blog-gridsome-base-dir nil
  "Netlify gridsome base directory")

(defvar org-journal-base-dir nil
  "Netlify gridsome base directory")

(defvar blog-gridsome-process "Gridsome Develop"
  "Name of 'gridsome develop' process process")

(setq blog-gridsome-base-dir (workspace-path "gridsome.org"))
(setq org-journal-base-dir (workspace-path "org/journal"))

(defun my-blog-gridsome-find-dir ()
  "Open gatsby blog files"
  (interactive)
  (find-file (workspace-path "gridsome.org/blog")))

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
    (setq daily-name (format-time-string "%Y-%m-%d"))
    (setq gridsome-post-subdir (ido-completing-read "Directory Name: " '("blog" "docs")))
  	(setq gridsome-post-base-name
          (or
           (org-global-prop-value "title")
           (read-from-minibuffer "File Name (Slug Format): " "name-your-blog")))
  	(setq gridsome-post-filepath
          (my-find-gridsome-filepath gridsome-post-subdir gridsome-post-base-name))
    (find-file gridsome-post-filepath)
    (goto-char (point-min))
    (insert "---\n")
    (insert (concat "title: "
                    (replace-regexp-in-string "-" " " gridsome-post-base-name)))
    (insert "\nauthor: Xing Wenju")
    (insert (concat "\ndate: " daily-name))
    (insert "\nexcerpt: \n---\n")
    (evil-save gridsome-post-filepath)))

;;;###autoload
(defun my-org-export-md-to-gridsome-newpost ()
	"Saving the current org file as a gridsome post
in a specific hugo post directory, will insert
front matter with the file title and date"
	(interactive)
	(progn
    (setq gridsome-post-subdir (ido-completing-read "Directory Name: " '("blog" "docs")))
  	(setq gridsome-post-base-name
          (or
           (org-global-prop-value "title")
           (read-from-minibuffer "File Name (Slug Format): " "name-your-blog")))
  	(setq gridsome-post-filepath
          (my-find-gridsome-filepath gridsome-post-subdir gridsome-post-base-name))
    (message gridsome-post-filepath)
    (org-md-export-as-markdown)
    (goto-char (point-min))
    (insert "---\n")
    (insert (concat "title: "
                    (replace-regexp-in-string "-" " " gridsome-post-base-name)))
    (insert "\nauthor: Xing Wenju\n")
    (insert (concat "date: "
                    (format-time-string "%Y-%m-%d")))
    (insert "\nexcerpt: \n---\n")
    (evil-save gridsome-post-filepath)
    (evil-window-delete)
    (find-file gridsome-post-filepath)))

;;;###autoload
(defun my-org-journal-new-journal ()
	"Create the journal in a specific directory, then your can export ..."
	(interactive)
	(progn
		(setq journal-file-path (concat org-journal-base-dir "/" (format-time-string "%Y-%m-%d") ".org"))
    (if (file-exists-p! journal-file-path)
        (find-file-other-window journal-file-path)
      (progn
        (find-file-other-window journal-file-path)
        (goto-char (point-min))
        (insert "---\n")
        (insert (concat "#+TITLE: Journal Entry\n"))
        (insert "#+AUTHOR: Xing Wenju\n")
        (insert (concat "#+DATE: " (format-time-string "%Y-%m-%d") "\n"))
        (insert "#+EXCERPT: org journal \n")
        (insert "---\n")))))

;;;###autoload
(defun commom--org-headers (file)
  "Return a draft org mode header string for a new article as FILE."
  (let ((datetimezone
         (concat
          (format-time-string "%Y-%m-%d"))))
    (concat
     "#+TITLE: " file
     "\n#+AUTHOR: "
     "\n#+DATE: " datetimezone
     "\n#+PUBLISHDATE: " datetimezone
     "\n#+EXCERPT: nil"
     "\n#+DRAFT: nil"
     "\n#+TAGS: nil, nil"
     "\n#+DESCRIPTION: Short description"
     "\n\n")))

;;;###autoload
(defun gridsome-current-time ()
  "Generate current time in date format at the frontmatter."
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%dT%T")
           (gridsome-orgtime-format (format-time-string "%z")))))

(defun gridsome--orgtime-format (x)
  "Format orgtime as X."
  (concat (substring x 0 3) ":" (substring x 3 5)))
