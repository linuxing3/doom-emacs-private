;;; lang-x/javascript/config.el -*- lexical-binding: t; -*-

(if (featurep! +tern) (load! "+tern"))
(if (featurep! +indium) (load! "+indium"))
(if (featurep! +bindings) (load! "+bindings"))
(if (featurep! +debug) (load! "+debug"))

(def-package! js2-mode
  :config
  (add-hook! '(js2-mode typescript-mode web-mode) #'prettier-js-mode)
  (map! :map js2-mode-map
        :localleader
        :n "sb" #'nodejs-repl-eval-buffer
        :n "sf" #'nodejs-repl-eval-function
        :n "sd" #'nodejs-repl-eval-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prettier-js
;; Modern js formatter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! prettier-js
  :commands (prettier-js-mode)
  :init
  (defun +javascript|setup-prettier-js ()
    "Sets up arguments and the mode."
    (interactive)
    (setq prettier-js-args '("--single-quote" "false"
                             "--trailing-comma" "all"
                             "--semi" "true"
                             "--bracket-spacing" "true"
                             "--tab-width"  "2"
                             ))
    (prettier-js-mode))
  :config
  (add-hook! '(tide-mode-hook js2-mode-hook typescript-mode-hook) #'+javascript|setup-prettier-js)
  (add-hook 'web-mode-hook #'(lambda ()
                                (my-enable-minor-mode
                                '("\\.(t|j)sx?\\'" . prettier-js-mode))))
  (map! :map* (json-mode-map js-mode-map js2-mode-map typescript-mode-map)
        :n  "="  #'prettier-js
        :n  "gp"  #'prettier-js))

(after! prettier-js
  (add-hook! '(js2-mode typescript-mode web-mode) #'prettier-js-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JsDoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-package! js-doc
  :config
  (setq js-doc-mail-address "linuxing3@qq.com"
        js-doc-author (format "linuxing3 <%s>" js-doc-mail-address)
        js-doc-url "https://xingwenju.netlify.com"
        js-doc-license "MIT")
  (add-hook! 'js2-mode-hook
    #'(lambda ()
        (define-key js2-mode-map "\C-cd" 'js-doc-insert-function-doc)
        (define-key js2-mode-map "@" 'js-doc-insert-tag))))
