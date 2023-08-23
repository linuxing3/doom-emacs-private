;;; lang-x/markdown/config.el -*- lexical-binding: t; -*-

(def-package! markdown-mode
  :init
  (when (featurep! +pandoc)
    (setq markdown-command "pandoc --from=markdown --to=html --standalone --mathjax --highlight-style=pygments")))
