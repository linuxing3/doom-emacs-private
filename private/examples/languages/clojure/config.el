;;; private/clojure/config.el -*- lexical-binding: t; -*-

(def-package! cider
  :hook (clojure-mode . cider-mode)
  :config
  (map! :map cider-mode-map
        :n  "c'"  #'cider-jack-in
        :n  "c\"" #'cider-jack-in-clojurescript
        :n  "cB"  #'cider-switch-to-repl-buffer
        :n  "cb"  #'cider-eval-buffer
        :n  "cn"  #'cider-repl-set-ns
        :n  "cj"  #'cider-find-var
        :n  "cd"  #'cider-doc
        :n  "cc"  #'cider-repl-clear-buffer
        :n  "cp"  #'cider-eval-sexp-at-point
        :n  "cr"  #'cider-eval-region))
