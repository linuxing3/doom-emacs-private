;;; c:/Users/Administrator/.doom.d/lang+python.el -*- lexical-binding: t; -*-
;;;
;;;
;;; FIXME ~/.doom.d/extensions/lang+python.el
;;(after! lsp-python-ms
;;  (set-lsp-priority! 'mspyls 1))
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
                                "D:/var/anaconda3"
                                "D:/lib/anaconda3"
                                "I:/lib/anaconda3"
                                "D:/var/anaconda2"
                                "C:/ProgramData/Anaconda3"
                                "~/.anaconda"
                                "~/.anaconda3"
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

(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))
