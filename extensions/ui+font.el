;;; private/themes/+font.el -*- lexical-binding: t; -*-

;;
;; Font
;;
(defvar my-font-list nil "A list of fonts for `my-cycle-font' to cycle from.")

(setq my-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "BlexMono Nerd Font-12"
          "Hack-12"
          "Hack Nerd-12"
          "Source Code Pro-12"
          "Source Code Pro Nerd-12"
          "Fira Code-12"
          "Fira Code Nerd-12"
          "JetBrains Nerd Mono-12"
          "Yahei Consolas Hybrid-12"
          "Microsoft Yahei-12"
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
          "IBM Plex Mono-12"
          "Monaco-12"
          "Courier-12"
          "Menlo-12"))))

(if (string-equal system-type "windows-nt")
    (setq
     doom-font (font-spec :family "IBM Plex Mono" :size 16)
     doom-big-font (font-spec :family "IBM Plex Mono" :size 18)
     doom-variable-pitch-font (font-spec :family "Microsoft Yahei" :size 16)
     doom-unicode-font (font-spec :famaly "Microsoft Yehei" :size 16))
	(setq
   doom-font (font-spec :family "IBM Plex Mono" :size 16)
   doom-big-font (font-spec :family "IBM Plex Mono" :size 18)
   doom-variable-pitch-font (font-spec :family "IBM Plex Mono" :size 16)
   doom-unicode-font (font-spec :family "IBM Plex Mono" :size 16)))

