;;; completion/config.el -*- lexical-binding: t; -*-


;;
;; Packages
;;

(def-package! auto-complete
  :when (featurep! :private autocomplete +ac)
  :commands ac-config-default
  :config
  (ac-config-default)
  (ac-set-trigger-key "TAB")
  )
