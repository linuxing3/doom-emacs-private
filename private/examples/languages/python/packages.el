;; -*- no-byte-compile: t; -*-
;;; private/python/packages.el

;; requires: python jedi setuptools

;; (package! pyenv-mode)
;; (package! pyvenv)
(package! yapfify)
(package! pipenv)

(when (featurep! +elpy)
  (package! elpy))

;; lsp
(cond ((and (featurep! :feature-x lsp)
            (featurep! +lsp))
       (package! lsp-python))
      ((package! anaconda-mode)
       (when (featurep! :completion company)
         (package! company-anaconda))))
