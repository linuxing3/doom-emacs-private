;;; c:/Users/Administrator/.doom.d/lang+python.el -*- lexical-binding: t; -*-
;;;
;;;
;;; FIXME ~/.doom.d/extensions/lang+python.el
;;; Enable anaconda
;;;
(use-package! conda
  :when (featurep! +conda)
  :after python
  :config
  ;; The location of your anaconda home will be guessed from a list of common
  ;; possibilities, starting with `conda-anaconda-home''s default value (which
  ;; will consult a ANACONDA_HOME envvar, if it exists).
  ;;
  ;; If none of these work for you, `conda-anaconda-home' must be set
  ;; explicitly. Afterwards, run M-x `conda-env-activate' to switch between
  ;; environments
  (or (cl-loop for dir in (list conda-anaconda-home
                                "C:/ProgramData/anaconda3"
                                "d:/var/anaconda3"
                                "d:/lib/anaconda3"
                                "d:/var/anaconda2"
                                "c:/ProgramData/Anaconda3"
                                "~/.anaconda"
                                "~/.miniconda"
                                "~/.miniconda3"
                                "~/miniconda3"
                                "/usr/bin/anaconda3"
                                "/usr/local/anaconda3"
                                "/usr/local/miniconda3"
                                "/usr/local/Caskroom/miniconda/base")
               if (file-directory-p dir)
               return (setq conda-anaconda-home dir
                            conda-env-home-directory dir))
      (message "Cannot find Anaconda installation")))

(setq conda-env-autoactivate-mode t)

(if (file-executable-p (expand-file-name "~/.local/bin/pipenv"))
    (setq pipenv-executable (expand-file-name "~/.local/bin/pipenv"))
  (setq pipenv-executable "~/.pipenv"))


;; Lsp Server
;; FIXME 不支持x86系统
;; (setq lsp-python-ms-base-url "https://pvsc.azureedge.net")
;; (setq lsp-python-ms-base-url "https://pvsc.blob.core.windows.net")
;; /python-language-server-stable?restype=container&comp=list&prefix=Python-Language-Server-win-x64"
;; (setq lsp-python-ms-dir (concat lsp-server-install-dir "mspyls/"))
;; (setq lsp-python-ms-python-executable (concat lsp-python-ms-dir "Microsoft.Python.LanguageServer" (if (eq system-type "windows-nt") ".exe" "")))
