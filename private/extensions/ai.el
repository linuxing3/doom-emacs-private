;;; private/extensions/ai.el -*- lexical-binding: t; -*-

(use-package aidermacs
  :bind (("C-c ." . aidermacs-transient-menu))
  :config
  (setq aidermacs-backend 'kitty)
  :custom
  ;; Default model used for all modes unless overridden
  (aidermacs-default-model "deepseek")
  ;; Optional: Set specific model for architect reasoning
  (aidermacs-architect-model "deepseek-coder")
  ;; Optional: Set specific model for code generation
  (aidermacs-editor-model "deepseek-coder"))
