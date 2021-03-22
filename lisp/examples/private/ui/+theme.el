;;; private/themes/+theme.el -*- lexical-binding: t; -*-

;; More beautiful themes
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" doom-private-dir))

(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/xing-one-theme-20180527.736" doom-private-dir))

;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :config
  (setq doom-theme 'doom-one-light))
