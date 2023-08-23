;;; private/writer/+blog.el -*- lexical-binding: t; -*-

(provide '+blog)

;;; Config

(defvar blog-dir nil
  "Hugo content directory")

(defvar blog-public-dir nil
  "Hugo output directory")

(defvar blog-hugo-process "Hugo Server"
  "Name of 'hugo server' process process")

(defvar blog-hugo-server-site "http://localhost:1313/"
  "URL for `blog-hugo-process'")

;; Netlify vuepress
(defvar blog-vuepress-dir nil
  "Netlify vuepress content directory")

(defvar blog-vuepress-base-dir nil
  "Netlify vuepress content directory")

(defvar blog-vuepress-public-dir nil
  "Netlify vuepres public output directory")

;; Netlify gatsby
(defvar blog-gatsby-base-dir nil
  "Netlify gatsby base directory")

(defvar blog-gatsby-process "Gatsby Develop"
  "Name of 'gatsby develop' process process")

;; Netlify gatsby
(defvar blog-gridsome-base-dir nil
  "Netlify gridsome base directory")

(defvar blog-gridsome-process "Gridsome Develop"
  "Name of 'gridsome develop' process process")

;;; Blog Commands

(defun my-blog-hugo-find-dir ()
  "Open hugo blog files"
  (interactive)
  (find-file (os-path "~/Dropbox/xingwenju.com/hugo/content")))

(defun my-blog-gridsome-find-dir ()
  "Open gatsby blog files"
  (interactive)
  (find-file (os-path "~/workspace/gridsome.org/blog")))

(defun my-blog-gatsby-find-dir ()
  "Open gatsby blog files"
  (interactive)
  (find-file (os-path "~/workspace/gatsby-starter-netlify-cms")))

(defun my-blog-hugo-deploy ()
  "Run hugo and push changes upstream."
  (interactive)
  (with-dir blog-public-dir
            (shell-command "git rm -rf .")
            (shell-command "git clean -fxd")
            (with-temp-file "CNAME"
              (insert "www.xingwenju.com\spacemacs"))

            (with-dir blog-dir (->> blog-public-dir
                                  (concat "hugo -d ")
                                  shell-command))

            (shell-command "git add .")
            (--> (current-time-string)
               (concat "git commit -m \"" it "\"")
               (shell-command it))
            (shell-command "git push -u origin master")
            ;; (magit-push-current-to-upstream nil)
			))

(defun my-blog-vuepress-deploy ()
  "Run netlify cli and push changes upstream."
  (interactive)
  (with-dir blog-vuepress-dir
			;; deploy to github for ci
            (shell-command "git add .")
            (--> (current-time-string)
               (concat "git commit -m \"" it "\"")
               (shell-command it))
            (shell-command "git push -u origin master")
  ))

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

(defun my-blog-gatsby-deploy ()
  "Run gatsby cli and push changes upstream."
  (interactive)
  (with-dir blog-gatsby-base-dir
			;; deploy to github for ci
            (shell-command "git add .")
            (--> (current-time-string)
               (concat "git commit -m \"" it "\"")
               (shell-command it))
            (shell-command "git push -u origin master")
  ))

(defun my-blog-hugo-start-server ()
  "Run hugo server if not already running and open its webpage."
  (interactive)
  (with-dir blog-dir
            (unless (get-process blog-hugo-process)
              (start-process blog-hugo-process nil "hugo" "server"))
            (browse-url blog-hugo-server-site)))

(defun my-blog-hugo-end-server ()
  "End hugo server process if running."
  (interactive)
  (--when-let (get-process blog-hugo-process)
    (delete-process it)))

(defun my-blog-gridsome-start-server ()
  "Run gridsome server if not already running and open its webpage."
  (interactive)
  (with-dir blog-gatsby-base-dir
            (unless (get-process blog-gridsome-process)
              (start-process blog-gridsome-process nil "gridsome" "develop" "-H" "0.0.0.0"))
			))

(defun my-blog-gatsby-start-server ()
  "Run gatsby server if not already running and open its webpage."
  (interactive)
  (with-dir blog-gatsby-base-dir
            (unless (get-process blog-gatsby-process)
              (start-process blog-gatsby-process nil "gatsby" "develop" "-H" "0.0.0.0"))
			))

(defun my-blog-gatsby-end-server ()
  "End gatsby server process if running."
  (interactive)
  (--when-let (get-process blog-gatsby-process)
    (delete-process it)))

(defun my-blog-gridsome-end-server ()
  "End gridsome server process if running."
  (interactive)
  (--when-let (get-process blog-gridsome-process)
    (delete-process it)))

;;; Ends
