;;; lang-x/plantuml/config.el -*- lexical-binding: t; -*-

(def-package! plantuml-mode
  :defer t
  :init
  (setq plantuml-jar-path (os-path "~/Dropbox/bin/plantuml.jar")
        org-plantuml-jar-path plantuml-jar-path)
  :config
  (set! :popup "^\\*PLANTUML" '((size . 0.4)) '((select) (transient . 0))))
