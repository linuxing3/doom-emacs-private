;;; private/themes/+font.el -*- lexical-binding: t; -*-
;;
;; UI Font and Window Configuration
;;
;; This file handles visual presentation including:
;; - Cross-platform font configuration
;; - Font cycling between multiple options
;; - Popup window rules for common buffers
;; - Modeline and dashboard tweaks
;;
;; Platform support:
;; - Windows: Yahei, Consolas, Fira Code
;; - Linux: DejaVu, Symbola  
;; - Mac: IBM Plex, Monaco, Menlo
;;
;; Key features:
;; - Automatic font selection per OS
;; - Font size presets (normal/big)
;; - Special popup rules for Org agenda/brain
;; - Dired mode visual tweaks
;;
;; See also:
;; - README.org for usage tips
;; - evil/config.el for cursor styling

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
          "Source Code Pro-12"
          "Source Code Pro Nerd-12"
          "Fira Code-12"
          "Fira Code Nerd-12"
          "JetBrainsMono Nerd Font-12"
          "DejaVu Sans Mono-12"
          "DejaVu Sans-12"
          "Symbola-12"
          ))
       ((string-equal system-type "darwin") ; Mac
        '(
          "JetBrainsMono Nerd Font-12"
          "Monaco-12"
          "Courier-12"
          "Menlo-12"))))

(if (string-equal system-type "windows-nt")
    (setq
     doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)
     doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
     doom-variable-pitch-font (font-spec :family "Microsoft Yahei" :size 16))
  (setq
   doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)
   doom-big-font (font-spec :family "JetBrainsMono Nerd Font" :size 18)
   doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font" :size 16)))

(custom-set-faces!
  `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))
