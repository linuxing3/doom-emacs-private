;;; c:/Users/Administrator/.doom.d/lang+python.el -*- lexical-binding: t; -*-

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
