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

(custom-set-faces!
  `(doom-modeline-bar-inactive :background ,(face-background 'mode-line-inactive)))

;; (setq +doom-dashboard-banner-file (expand-file-name "logo.png" doom-private-dir))
(defun linuxing3/dired-mode-setup ()
  "to be run as hook for `dired-mode'."
  (dired-hide-details-mode 1))
(add-hook 'dired-mode-hook 'linuxing3/dired-mode-setup)

(set-popup-rule! "^\\*Org Agenda" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^CAPTURE.*\\.org$" :side 'bottom :size 0.90 :select t :ttl nil)
(set-popup-rule! "^\\*org-brain" :side 'right :size 1.00 :select t :ttl nil)
