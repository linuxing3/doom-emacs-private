;;; private/python/config.el -*- lexical-binding: t; -*-

;;; Bindings
(if (featurep! +bindings) (load! +bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Native python mode in doom modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! python
  :when (featurep! :lang python)
  :init
  (if IS-WINDOWS
      (setq python-shell-interpreter "G:/var/anaconda3/python.exe"
            python-shell-interpreter-args
            "-i G:/var/anaconda3/Scripts/ipython-script.py")
    (setq python-shell-interpreter "ipython"))
  (if IS-WINDOWS
    (setq python-shell-exec-path '(
                                   "G:/var/anaconda3"
                                   "G:/var/anaconda3/Scripts"
                                   "E:/var/python2.7"
                                   "E:/var/python2.7/Scripts"))
    (setq python-shell-exec-path '(
                                    "/anaconda3/bin"
                                    "/user/local/bin"))))

(def-package! conda
  :when (featurep! :lang python +conda)
  :after python
  :config
  (if IS-MAC
      (setq conda-anaconda-home "/anaconda3")
    (setq conda-anaconda-home "G:/var/anaconda3")))

(def-package! yapfify
  :when (featurep! :lang python)
  :commands (yapf-mode yapfify-buffer)
  :init
  (add-hook! 'python-mode-hook 'yapf-mode)
  (map! :map python-mode-map
        :localleader
        :n "=" #'yapfify-buffer))

(def-package! pipenv
  :when (featurep! :lang python)
  :after python
  :commands pipenv-project-p
  :hook (python-mode . pipenv-mode)
  :init (setq pipenv-with-projectile nil)
  (add-hook 'python-mode-hook 'pipenv-mode)
  (setq
   pipenv-projectile-after-switch-function #'pipenv-projectile-after-switch-extended)
  :config
  (if IS-MAC
      (setq pipenv-executable "/anaconda3/bin/pipenv")
    (setq pipenv-executable "G:\\var\\anaconda3\\bin\\pipenv"))
  (map! :map python-mode-map
        :localleader
        :n "a" #'pipenv-activate
        :n "d" #'pipenv-deactivate
        :n "s" #'pipenv-deactivate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using lsp as alternative IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (featurep! +lsp)
  (after! python
    (lsp-define-stdio-client lsp-python "python"
                             #'projectile-project-root
                             '("pyls"))
    ;; 调用 pyls 既上边安装的 Python Language Server
    ;; 不需要安装 lsp-python, 以下几行提供与 lsp-python 相同的功能
    ;; (lsp-define-stdio-client lsp-python "python"
    ;;         (lsp-make-traverser #'(lambda (dir)
    ;;                     (directory-files
    ;;                     dir
    ;;                     nil
    ;;                     "\\(__init__\\|setup\\)\\.py\\|Pipfile")))
    ;;         '("pyls"))
    (add-hook! python-mode #'lsp-python-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using elpy as alternative IDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! elpy
  :when (featurep! +elpy)
  :config
  (defalias 'workon 'pyvenv-workon)
  (elpy-enable))

(def-package! jedi
  :when (featurep! +elpy)
  :init
  (add-hook 'python-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook 'jedi:ac-setup))

;; ein
;;; ein:jupyter-server-start; --- Start a jupyter server within emacs
;;; ein:notebooklist-login; --- Login to an existing jupyter server
;;; ein:notebooklist-open; --- Open the list of jupyter notebooks
(def-package! ein
  :when (featurep! :tools ein)
  :config
  ;; notebook directory
  (setq +ein-notebook-dir "~/Dropbox/shared")
  (after! ein-jupyter)
  ;; server command
  (if IS-WINDOWS
      (setq ein:jupyter-default-server-command "G:/var/anaconda3/Scripts/jupyter-notebook.exe")
    (setq ein:jupyter-default-server-command "jupyter-notebook"))
  ;; notebook direcoty
  (setq ein:jupyter-default-notebook-directory "~/Dropbox/shared/")
  (map! :leader
        (:desc "Extra" :prefix "z"
	 	      :desc "Jupyter server start" :nv "s" #'ein:run
	 	      :desc "Jupyter run eval buffer" :nv "e" #'ein:connect-run-or-eval-buffer
		      :desc "Jupyter server login" :nv "l" #'ein:notebooklist-login
		      :desc "Jupyter server open" :nv "o" #'ein:notebooklist-open)))
;; submodules
(def-package! ein-notebook)
(def-package! ein-subpackages)
