;;; lang-x/typescript/config.el -*- lexical-binding: t; -*-

;; Javascript, Typescript and support for lsp-mode
;;
;; Install:
;;
;; npm install -g typescript
;; npm install -g typescript-language-server
;;

(def-package! typescript-mode
  :init
  (defun ts-nodejs-repl ()
    "Start a JavaScript REPL to be evaluated in the visiting browser."
    (interactive)
    (when (not (get-buffer "*ts-nodejs-repl*"))
      (with-current-buffer (get-buffer-create "*ts-nodejs-repl*")
        ;; TODO define tsnode repl mode
        (ts-nodejs-repl-mode)))
    (pop-to-buffer (get-buffer "*ts-nodejs-repl*")))
  (defun +typescript/repl ()
    "Open a Typescript REPL with ts-node."
    (interactive)
    (call-interactively
    (if (and (featurep 'skewer-mode)
              (or skewer-mode skewer-css-mode skewer-html-mode))
        #'skewer-repl
      #'ts-nodejs-repl)))
  :config
  (set-repl-handler! 'typescript-mode #'+typescript/repl))

(after! typescript-mode
  (add-hook 'typescript-mode-hook #'rainbow-delimiters-mode)
  (setq-hook! 'typescript-mode-hook
    comment-line-break-function #'js2-line-break)
  (set-electric! 'typescript-mode
    :chars '(?\} ?\)) :words '("||" "&&"))
  (set-docsets! 'typescript-mode "TypeScript" "AngularTS"))

;; Sub-modules
;; (when (featurep! +lsp) (load! +lsp))
(def-package! lsp-typescript
  :when (featurep! +lsp)
  :when (featurep! :feature-x lsp)
  :hook ((js2-mode web-mode typescript-mode) . lsp-typescript-enable))

(def-package! tide
  :defer t
  :init
  (add-hook! (js2-mode typescript-mode rxjs-mode) #'my-setup-tide-mode)
  (add-hook 'web-mode-hook #'my-setup-tide-mode)
  :config
  (setq tide-completion-detailed t
        tide-always-show-documentation t)
  (map! :map tide-mode-map
      :localleader
      :n "R" #'tide-restart-server
      :n "E" #'tide-project-errors
      :n "D" #'tide-documentation-at-point
      :n "o" #'tide-organize-imports
      :n "d" #'tide-jsdoc-template
      :n "F" #'tide-refactor
      :n "X" #'tide-fix
      :n "s" #'tide-rename-symbol
      :n "f" #'tide-rename-file))
