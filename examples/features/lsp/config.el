;;; feature-x/lsp/config.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :after (:any cc-mode
               c-mode
               c++-mode
               objc-mode
               python
               haskell-mode
               rust-mode
               caml-mode
               typescript-mode
               js2-mode
               web-mode
               vue-mode
               css-mode
               scss-mode
               sass-mode
               less-mode
               stylus-mode)
  :commands (lsp-mode lsp-define-stdio-client)
  :config
  (setq lsp-enable-eldoc t)
  (setq lsp-enable-completion-at-point t))

(def-package! lsp-imenu
    :init
    ;; 启用 lsp-imenu 集成
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(def-package! lsp-ui
  :after lsp-mode
  :config
  ;; hook in config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  (setq lsp-ui-flycheck-enable t)
  (setq lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-sideline-ignore-duplicate t)
  ;; navigation
  (set-lookup-handlers! 'lsp-ui-mode
    :definition #'lsp-ui-peek-find-definitions
    :references #'lsp-ui-peek-find-references)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(def-package! company-lsp
  :after lsp-mode
  :config
  (set-company-backend! 'lsp-mode 'company-lsp)
  (setq company-lsp-async t)
  (setq company-lsp-enable-recompletion t)
  (setq company-lsp-enable-snippet t))

(def-package! company-quickhelp)

(def-package! company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-downcase nil)
  (add-hook 'company-mode-hook 'company-quickhelp-mode)
  (add-to-list 'company-backends 'company-lsp))
