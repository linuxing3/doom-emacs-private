;; -*- no-byte-compile: t; -*-
;;; completion/packages.el

(when (featurep! :private autocomplete +ac)
  (package! auto-complete))
