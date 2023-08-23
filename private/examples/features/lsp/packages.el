;; -*- no-byte-compile: t; -*-
;;; feature-x/lsp/packages.el

(package! lsp-mode)
(package! lsp-ui)

(when (featurep! :completion company)
  (package! company-lsp)
  (package! company-quickhelp))
