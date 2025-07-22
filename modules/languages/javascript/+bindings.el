;;; lang-x/javascript/+bindings.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! (:after js2-mode
        :map js2-mode-map
        :localleader
        :n "zc" #'js2-mode-hide-element
        :n "zo" #'js2-mode-show-element
        :n "zr" #'js2-mode-show-all
        :n "ze" #'js2-mode-toggle-element
        :n "zF" #'js2-mode-toggle-hide-functions
        :n "zC" #'js2-mode-toggle-hide-comments
        :nr "=" #'prettier-js
        :nr "3" #'+javascript/refactor-menu
        :n  "1" #'+javascript/repl
        :n  "2" #'+javascript/skewer-this-buffer))

(map! (:after skewer-mode
        :map skewer-mode-map
        :localleader
        :n "sb" #'skewer-load-buffer
        :n "sf" #'skewer-eval-defun
        :n "se" #'skewer-eval-last-expression
        :n "sp" #'skewer-eval-print-last-expression
        :n "sr" #'skewer-repl
        :n "sR" #'my-skewer-start-repl
        :n "sB" #'my-skewer-load-buffer-and-focus
        :n "sF" #'my-skewer-eval-defun-and-focus
        :n "sS" #'my-skewer-eval-region-and-focus
        :n "sP" #'my-skewer-eval-region))
