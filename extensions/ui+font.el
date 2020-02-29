;;; private/themes/+font.el -*- lexical-binding: t; -*-

;;
;; Font
;;
(defvar my-font-list nil "A list of fonts for `my-cycle-font' to cycle from.")

(setq my-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Yahei Consolas Hybrid-16"
          "Microsoft Yahei-16"
          "Fira Code-16"
          "Hack-16"
          "Courier-16"
          "Lucida Console-16"
          "Segoe UI Symbol-16"
          "Lucida Sans Unicode-16"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "DejaVu Sans Mono-16"
          "DejaVu Sans-16"
          "Symbola-14"
          ))
       ((string-equal system-type "darwin") ; Mac
        '(
          "Fira Code-16"
          "Monaco-16"
          "Courier-16"
          "Menlo-16"))))

(if (string-equal system-type "windows-nt")
	(setq
    doom-font (font-spec :family "Hack" :size 16)
    doom-big-font (font-spec :family "Yahei Consolas Hybrid" :size 18)
		doom-variable-pitch-font (font-spec :family "Yahei Consolas Hybrid" :size 16)
		doom-unicode-font (font-spec :family "Yahei Consolas Hybrid" :size 16))
	(setq
    doom-font (font-spec :family "Fira Code" :size 16)
    doom-big-font (font-spec :family "Fira Code" :size 18)
		doom-variable-pitch-font (font-spec :family "Fira Code" :size 16)
		doom-unicode-font (font-spec :family "Fira Code" :size 16)))
