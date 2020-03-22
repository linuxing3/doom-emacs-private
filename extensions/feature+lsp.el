;;; ~/.doom.d/extensions/feature+lsp.el -*- lexical-binding: t; -*-

(use-package! lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-enable-indentation t
        lsp-enable-on-type-formatting t
        lsp-enable-symbol-highlighting t
        lsp-enable-file-watchers t

        ;; lsp-ui-doc is redundant with and more invasive than
        ;; `+lookup/documentation'
        lsp-ui-doc-max-height 8
        lsp-ui-doc-max-width 35
        lsp-ui-doc-enable t
        lsp-ui-doc-border "green"
        lsp-ui-doc-header t

        ;; FIXME lsp-ui-sideline is redundant with eldoc and much more invasive, so
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil)

  (when (featurep! +peek)
    (set-lookup-handlers! 'lsp-ui-mode :async t
      :definition 'lsp-ui-peek-find-definitions
      :references 'lsp-ui-peek-find-references)))
