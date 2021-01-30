;;; c:/Users/wjb/.doom.d/app+plantuml.el -*- lexical-binding: t; -*-

(setq plantuml-jar-path (concat doom-private-dir "bin/plantuml.jar")
      org-plantuml-jar-path plantuml-jar-path)

(setq org-ditaa-jar-path (concat doom-private-dir "bin/ditaa.jar"))

(use-package! fzf)
