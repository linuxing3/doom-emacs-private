;;; private/themes/+font.el -*- lexical-binding: t; -*-

;;
;; Font
;;
(defvar my-font-list nil "A list of fonts for `my-cycle-font' to cycle from.")

(setq my-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Yahei Consolas Hybrid-12"
          "Microsoft Yahei-12"
          "Fira Code-12"
          "Hack-12"
          "Courier-12"
          "Lucida Console-12"
          "Segoe UI Symbol-12"
          "Lucida Sans Unicode-12"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "DejaVu Sans Mono-12"
          "DejaVu Sans-12"
          "Symbola-12"
          ))
       ((string-equal system-type "darwin") ; Mac
        '(
          "Fira Code-12"
          "Monaco-12"
          "Courier-12"
          "Menlo-12"))))

(if (string-equal system-type "windows-nt")
	(setq
    doom-font (font-spec :family "Hack" :size 12)
    doom-big-font (font-spec :family "Yahei Consolas Hybrid" :size 18)
		doom-variable-pitch-font (font-spec :family "Yahei Consolas Hybrid" :size 12)
		doom-unicode-font (font-spec :family "Yahei Consolas Hybrid" :size 12))
	(setq
    doom-font (font-spec :family "Fira Code" :size 12)
    doom-big-font (font-spec :family "Fira Code" :size 18)
		doom-variable-pitch-font (font-spec :family "Fira Code" :size 12)
		doom-unicode-font (font-spec :family "Fira Code" :size 12)))

(custom-set-faces!
  `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))
