;;; lang-x/typescript/config.el -*- lexical-binding: t; -*-

;; Javascript, Typescript and support for lsp-mode
;;
;; Install:
;;
;; npm install -g typescript
;; npm install -g typescript-language-server
;;

(def-package! lsp-typescript
  :when (featurep! +lsp)
  :when (featurep! :feature-x lsp)
  :hook ((js2-mode web-mode typescript-mode) . lsp-typescript-enable))
