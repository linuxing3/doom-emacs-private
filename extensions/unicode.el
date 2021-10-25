;;; extensions/unicode.el -*- lexical-binding: t; -*-

;;use unicode everywhere
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8-unix)
(modify-coding-system-alist 'process "*" 'utf-8-unix)
(set-buffer-file-coding-system 'utf-8-unix)
(set-file-name-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-language-environment "UTF-8")
(setq locale-coding-system 'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;;windows没有启用unicode时，中文语言是gbk编码gb18030会导致有些中文字符找不到字体
(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'chinese-gbk))

;;The clipboard on windows dose not play well with utf8
(unless (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

;; 英文日期，会影响日期格式
(setq system-time-locale "C")
