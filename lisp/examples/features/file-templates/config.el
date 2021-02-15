;;; private/file-templates/config.el -*- lexical-binding: t; -*-

(require! :feature snippets)

;; File templates
(setq private-file-templates-dir
  (expand-file-name "templates/" (file-name-directory load-file-name))
  "The path to a directory of yasnippet folders to use for file templates.")

(after! yasnippet
 (if (featurep! :feature snippets)
   (add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)
   (set-file-template! "\\.vue$" ':trigger "__.vue" :mode web-mode)
   (yas-reload-all)))

;; private/file-templates/config.el ends here
