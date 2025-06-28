;;; private/writer/autoload/blog.el -*- lexical-binding: t; -*-

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gridsome Post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
     (insert (concat "title: " gridsome-post-filename "\n"))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gatsby Post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
 (defun my-gatsby-create-newpost-empty (gatsby-post-subdir)
	"Saving the current buffer as a gatsby post
in a specific hugo post directory"
	(interactive
		(let ((gatsby-post-subdirs
						'("blog" "doc" )))
				(list (ido-completing-read "Directory Name:" gatsby-post-subdirs))
		))
	(progn
			(setq daily-name (format-time-string "%Y-%m-%d"))
			(setq gatsby-post-title
				(read-from-minibuffer "请输入文件名：" (concat daily-name "-filename.md" )))
			(setq gatsby-post-dir
				(concat blog-gatsby-base-dir "/src/pages/" gatsby-post-subdir "/"))
			(find-file (concat gatsby-post-dir gatsby-post-title))
			(my-hugo-insert-org-format-title)
	))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hugo Post
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; 转markdown文件为hugo markdown

;;;###autoload
 (defun my-change-md-to-hugo (from-file-name directory)
  "Recursively change md files to hugo files"
  (interactive)
  (let ((to-file-name from-file-name))
			(find-file (concat directory "/" from-file-name))
			(goto-char (point-min))
			(insert "+++\n")
			(insert (concat "title=\"" from-file-name "\"\n"))
			(insert "date=\"2018-02-16T14:05:34-04:00\"\n")
			(insert "+++\n")
      (goto-char (point-min))
			(write-file (concat directory "/" to-file-name))
    ))

;;;###autoload
 (defun my-walk-and-change-md-to-hugo (directory)
	"Walk a directory and apply function to each file"
  (interactive)
  (mapcar
		(lambda (it) ;; function to apply
			(unless (or (string= it ".") (string= it ".."))
				(unless (file-directory-p it)
						(funcall 'my-change-md-to-hugo it directory)))) ;; end function
				(directory-files directory)) ;; list to itarate
				)

;;;###autoload
 (defun my-hugo-insert-org-format-title ()
  "Insert's the org title in a hugo post."
  (interactive)
  ;;(let ((datim (format-time-string "%Y%m%d%H%M%S%N")))
	(let ((datim (format-time-string "%FT%T%z")))
    (goto-char (point-min))
    (insert "---\n")
    (insert "\ntemplateKey: blog-post|doc-page\n")
    (insert "title: \"")
		(insert (buffer-name))
    (insert "\"\n")
    (insert "author: \"Xing Wenju\"\n")
    (insert "description: \"Some Description\"\n")
    (insert "date: ")
    (insert datim )
    (insert "\ntags:\n - tag1")
    (insert "\n---\n")
    ))

;;;###autoload
 (defun my-hugo-create-newpost-empty (hugo-post-subdir)
	"Saving the current buffer as a hugo post
in a specific hugo post directory"
	(interactive
		(let ((hugo-post-subdirs
						'("emacs" "linux" "hugo" "nodejs" "python" "vim" "vms")))
				(list (ido-completing-read "Directory Name:" hugo-post-subdirs))
		))
	(progn
			(setq hugo-post-title
				(read-from-minibuffer "请输入文件名：" (buffer-name)))
			(setq hugo-post-dir
				(concat easy-hugo-basedir easy-hugo-postdir "/" hugo-post-subdir "/"))
			(find-file (concat hugo-post-dir hugo-post-title))
	)
	)

;;;###autoload
 (defun my-hugo-smart-create-newpost-with-current-buffer (post-file)
" Extended from easy-hugo-newpost
file needs to have and extension '.md' or '.org' or '.ad' or '.rst' or '.mmark' or '.html'."
  (interactive (list (read-from-minibuffer
		      "输入文件名: "
		      `(,easy-hugo-default-ext . 1) nil nil nil)))
  (easy-hugo-with-env
   (let (
         (filename (expand-file-name post-file easy-hugo-postdir)) ;; file to create
         (file-ext (file-name-extension post-file)) ;; file extention
         (current-buffer-file-name (buffer-file-name)) ;; buffer to copy content
         )
     ;; Check file extention
     (when (not (member file-ext easy-hugo--formats))
       (error "please enter .%s or .org or .%s or .rst or .mmark or .%s file name"
	      easy-hugo-markdown-extension
	      easy-hugo-asciidoc-extension
	      easy-hugo-html-extension))
     ;; Check target file exists
     (when (file-exists-p (file-truename filename))
       (error "%s already exists!" filename))
     ;; Check if org is supported and defined
     (if (and (null easy-hugo-org-header)
	      (<= 0.25 (easy-hugo--version)))
	 (call-process "hugo" nil "*hugo*" t "new"
		       (file-relative-name filename
					   (expand-file-name "content" easy-hugo-basedir)))
       (progn
	 (if (or (string-equal file-ext easy-hugo-markdown-extension)
		 (string-equal file-ext easy-hugo-asciidoc-extension)
		 (string-equal file-ext "rst")
		 (string-equal file-ext "mmark")
		 (string-equal file-ext easy-hugo-html-extension))
	     (call-process "hugo" nil "*hugo*" t "new"
			   (file-relative-name filename
					       (expand-file-name "content" easy-hugo-basedir))))))
     ;; End of check org support

     ;; Kill temporary buffer
     (when (get-buffer "*hugo*")
       (kill-buffer "*hugo*"))

     ;; Open the new post with filename
     (find-file filename)
     ;; Insert Header if has template
     (when (or easy-hugo-org-header
	       (and (> 0.25 (easy-hugo--version))
		    (string-equal file-ext "org")))
       ;; Insert header
			 (goto-char (point-min))
			 (insert "\n")
       (insert (easy-hugo--org-headers (file-name-base post-file)))
			 (insert "\n"))
		 (goto-char (point-max))
		 (insert "\n")
		 (insert-file-contents current-buffer-file-name)
		 (insert "\n")
		 ;; Save buffer
     (save-buffer)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Hotspots and Blogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
 (defun my-helm-hotspots ()
  "helm interface to my hotspots, which includes my locations,
org-files and bookmarks"
  (interactive)
  (helm :buffer "*helm: utities*"
        :sources `(,(my-hotspots-sources))))

;;;###autoload
 (defun my-hotspots-sources ()
  "Construct the helm sources for my hotspots"
  `((name . "Mail and News")
    (candidates . (("Calendar" . (lambda ()  (browse-url "https://www.google.com/calendar/render")))
                   ("RSS" . elfeed)
                   ("Blog" . blog-admin-start)
                   ("Github" . (lambda() (helm-github-stars)))
                   ("Calculator" . (lambda () (helm-calcul-expression)))
                   ("Run current flie" . (lambda () (my-run-current-file)))
                   ("Agenda" . (lambda () (org-agenda "" "a")))
                   ("sicp" . (lambda() (browse-url "http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-4.html#%_toc_start")))))
    (candidate-number-limit)
    (action . (("Open" . (lambda (x) (funcall x)))))))
