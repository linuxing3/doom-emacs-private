;;; c:/Users/Administrator/.doom.d/lang+python.el -*- lexical-binding: t; -*-
;;;
;;;
;;; FIXME ~/.emacs.d/modules/lang/python.config
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
                                "d:/var/anaconda3"
                                "d:/lib/anaconda3"
                                "d:/var/anaconda2"
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
      (message "Cannot find Anaconda installation"))

(if (and IS-WINDOWS (file-exists-p! "d:/lib/anaconda3"))
    (progn
      (setq conda-anaconda-home "D:/lib/anaconda3/")
      (setq conda-env-home-directory (expand-file-name "envs" conda-anaconda-home "envs")))
  (progn
    (setq conda-anaconda-home (expand-file-name (or (getenv "ANACONDA_HOME") "~/.anaconda3/")))
    (setq conda-env-home-directory (expand-file-name "envs" conda-anaconda-home))))

(setq conda-env-autoactivate-mode t)

(if (file-executable-p (expand-file-name "~/.local/bin/pipenv"))
    (setq pipenv-executable (expand-file-name "~/.local/bin/pipenv"))
  (setq pipenv-executable "pipenv"))

(setq lsp-python-ms-base-url "https://pvsc.azureedge.net")
