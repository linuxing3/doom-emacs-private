;;; extensions/feature+packages.el -*- lexical-binding: t; -*-

;; Usage
;; (setq package-archives '(("myelpa" . "~/myelpa/")))
;;
(use-package! elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/.myelpa"))
