;;; private/writer/autoload/project.el -*- lexical-binding: t; -*-

(require 'cl)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: defmacro to generate similar functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun +javascript|setup-gatsby-project ()
  "Search the current buffer's parent directories for `src`.
If it's found, then generate ctags."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "src"))
            (path (expand-file-name "src" root)))
      (progn
        (with-dir root
                  (shell-command "ctags -eR src/"))
        (when doom-debug-mode
          (message "Found Gatsby Project with %s" path)))
    (when doom-debug-mode
      (message "Not found src dir in %s" root))))

;;;###autoload
(defun +javascript|setup-netlify-project ()
  "Search the current buffer's parent directories for `netlify`.
If it's found, then generate ctags."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "src/cms"))
            (path (expand-file-name "src/cms" root)))
      (progn
        (with-dir root
                  (shell-command "ctags -eR src/"))
        (when doom-debug-mode
          (message "Found Netlify Project %s" path)))
    (when doom-debug-mode
      (message "Not found src dir in %s" root))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-known-projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun my-add-known-project ()
  (interactive)
  (progn
    (projectile-add-known-project (os-path "~/.emacs.d"))
    (projectile-add-known-project (os-path "~/.spacemacs.d"))
    (projectile-add-known-project (os-path "~/.vim"))
    (projectile-add-known-project (os-path "~/.fzf"))
    (projectile-add-known-project (os-path "~/dotfiles"))
    (projectile-add-known-project (os-path "~/go"))
    (projectile-add-known-project (os-path "~/journal"))
    (projectile-add-known-project (os-path "~/.config/doom/modules/private/xingwenju"))
    (projectile-add-known-project (os-path "~/.config/doom"))
    (projectile-add-known-project (os-path "~/workspace/cp-work-puppeteer"))
    (projectile-add-known-project (os-path "~/workspace/cp-work-awesome-vue"))
    (projectile-add-known-project (os-path "~/workspace/cp-work-parcel-vuejs"))
    (projectile-add-known-project (os-path "~/workspace/cp-work-vue-template"))
    (projectile-add-known-project (os-path "~/workspace/cp-work-react-app"))
    (projectile-add-known-project (os-path "~/workspace/express-apollo-server"))
    (projectile-add-known-project (os-path "~/workspace/micro-apollo-server"))
    (projectile-add-known-project (os-path "~/workspace/gatsby-netlify-cms-template"))
    (projectile-add-known-project (os-path "~/workspace/wechaty"))
    (projectile-add-known-project (os-path "~/workspace/wechaty-master"))
    (projectile-add-known-project (os-path "~/Dropbox/xingwenju.com/hugo"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/Presentations"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/InformationCenter"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/wx-robot"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/VeNews"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/homework"))
    (projectile-add-known-project (os-path "~/Dropbox/shared/daniel-homework"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; 标签和智能查找
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun my-create-tags-if-needed (SRC-DIR &optional FORCE)
  "Create tag for intellisense and return the full path of tags file"
  (let ((dir (file-name-as-directory (file-truename SRC-DIR)))
        file)
    (setq file (concat dir "TAGS"))
    (when (or FORCE (not (file-exists-p file)))
      (message "Creating TAGS in %s ..." dir)
      (shell-command
       (format "ctags -f %s -e -R %s" file dir)))
    file))

;;;###autoload
(defun my-update-tags ()
  (interactive)
  "check the tags in tags-table-list and re-create it"
  (dolist (tag tags-table-list)
    (my-create-tags-if-needed (file-name-directory tag) t)))


;;;###autoload
(defun my-auto-update-tags-when-save (prefix)
  (interactive "P")
  (cond
   ((not my-tags-updated-time)
    (setq my-tags-updated-time (current-time)))

   ((and (not prefix)
         (< (- (float-time (current-time)) (float-time my-tags-updated-time)) 300))
    ;; < 300 seconds
    (message "no need to update the tags")
    )
   (t
    (setq my-tags-updated-time (current-time))
    (my-update-tags)
    (message "updated tags after %d seconds." (- (float-time (current-time)) (float-time my-gtags-updated-time))))))

;;;;; 版本控制

;;;###autoload
(defun my-git-project-root ()
  "Return the project root for current buffer."
  (let ((directory default-directory))
    (locate-dominating-file directory ".git")))

;;;;; Return the project root for current buffer.

(defadvice persp-switch (after quit-helm-perspectives activate)
  (setq hydra-deactivate t))

;;;###autoload
(defun my-open-file-with-projectile-or-counsel-git ()
  (interactive)
  (if (my-git-project-root)
      (counsel-git)
    (if (projectile-project-p)
        (projectile-find-file)
      (counsel-file-jump))))

;;;###autoload
(defun my-find-file-in-git-repo (repo)
  (if (file-directory-p repo)
      (let* ((default-directory repo)
             (files (split-string (shell-command-to-string (format "cd %s && git ls-files" repo)) "\n" t)))
        (ivy-read "files:" files
                  :action 'find-file
                  :caller 'find-file-in-git-repo))
    (message "%s is not a valid directory." repo)))

;;;###autoload
(defun my-open-file-in-external-app (file)
  "Open file in external application."
  (interactive)
  (let ((default-directory (my-git-project-root))
        (file-path file))
    (if file-path
        (cond
         (IS-WINDOWS (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
         (IS-MAC (shell-command (format "open \"%s\"" file-path)))
         (IS-LINUX (let ((process-connection-type nil))
                     (start-process "" nil "xdg-open" file-path))))
      (message "No file associated to this buffer."))))
;;;;; 查找文件名
;;;###autoload
(defun my-project-name-contains-substring (REGEX)
  (let ((dir (if (buffer-file-name)
                 (file-name-directory (buffer-file-name))
               "")))
    (string-match-p REGEX dir)))

;;;; 项目
;;;;; 设置开发环境
;;;;;
;;;###autoload
(defun my-setup-develop-environment (workspace workdir)
  (interactive)
  (if
      (and (my-project-name-contains-substring workspace) (my-project-name-contains-substring workdir))
      (setq tags-table-list (list (my-create-tags-if-needed (concat "~/" workspace "/" workdir))))
    (message "Directory is Wrong!")
    ))


;;;###autoload
(defun my-github-browse-file--relative-url ()
  "Return \"username/repo\" for current repository.

Error out if this isn't a GitHub repo."
  (require 'vc-git)
  (let ((url (vc-git--run-command-string nil "config" "remote.origin.url")))
    (unless url (error "Not in a GitHub repo"))
    (when (and url (string-match "github.com:?/?\\(.*\\)" url))
      (replace-regexp-in-string "\\.git$" "" (match-string 1 url)))))

;;;###autoload
(defun my-github-browse-commit ()
  "Show the GitHub page for the current commit."
  (interactive)
  (let* ((commit git-messenger:last-commit-id)
         (url (concat "https://github.com/"
                      (github-browse-file--relative-url)
                      "/commit/"
                      commit)))
    (browse-url url)
    (git-messenger:popup-close)))

;;;###autoload
(defun my-search-in-workspace ()
  (interactive)
  (helm-do-ag (expand-file-name "~/workspace")))

;;;###autoload
(defun my-unwind-git-timemachine ()
  (if (not (eq last-command-event 13))
      (git-timemachine-quit)))

;;;###autoload
(defun my-magit-visit-pull-request ()
  "Visit the current branch's PR on GitHub."
  (interactive)
  (let ((remote-branch (magit-get-current-branch)))
    (cond
     ((null remote-branch)
      (message "No remote branch"))
     (t
      (browse-url
       (format "https://github.com/%s/pull/new/%s"
               (replace-regexp-in-string
                "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                (magit-get "remote"
                           (magit-get-remote)
                           "url"))
               remote-branch))))))
