;;; private/writer/+translate.el -*- lexical-binding: t; -*-

(def-package! company-english-helper
  :config
  (setq company-english-helper-fuzz-search-p t))

;;
;; 执行 insert-translated-name-insert 命令进入输入激活模式
;; 正常输入中文
;; 中文后输入空格自动查询翻译并替换成符合当前语言风格的变量名(或函数名)
;;
;; 如果当前光标在注释或者字符串区域, 会自动插入英文注释:
;; 执行 insert-translated-name-insert 命令进入输入激活模式
;; 正常输入中文
;; 中文后输入空格自动查询翻译并替换成英文注释
(def-package! insert-translated-name)

(def-package! youdao-dictionary
  :config
  (setq url-automatic-caching t)
  (global-set-key (kbd "C-c y") 'youdao-dictionary-search-at-point)
  (setq youdao-dictionary-search-history-file "~/.emacs.d/.youdao")
  (setq youdao-dictionary-use-chinese-word-segmentation t))
