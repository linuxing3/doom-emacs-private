;;; private/go/config.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;
(def-package! company-go
  :when (featurep! :completion company)
  :init (setq command-go-gocode-command "gocode")
  :after go-mode
  :config
  (setq company-go-show-annotation t)
  (set! :company-backend 'go-mode '(company-go)))
