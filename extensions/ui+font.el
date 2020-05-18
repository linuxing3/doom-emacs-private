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
          "Hack-12"
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
          "Fira Code-12"
          "Monaco-12"
          "Courier-12"
          "Menlo-12"))))

(if (string-equal system-type "windows-nt")
    (setq
     doom-font (font-spec :family "BlexMono Nerd Font" :size 16)
     doom-big-font (font-spec :family "BlexMono Nerd Font" :size 18)
     doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font" :size 16)
     doom-unicode-font (font-spec :famaly "Yahei Consolas Hybrid" :size 16))
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
