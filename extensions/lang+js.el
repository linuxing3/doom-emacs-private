;;; c:/Users/Administrator/.doom.d/lang+js.el -*- lexical-binding: t; -*-
;;;
;;;
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(add-hook!
 js2-mode 'prettier-js-mode
 (add-hook 'before-save-hook #'refmt-before-save nil t))

(setq
 typescript-indent-level 2
 css-indent-offset 2
 json-reformat:indent-width 2
 prettier-js-args '("--single-quote")
 web-mode-markup-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-css-indent-offset 2)
