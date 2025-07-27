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

(use-package dirvish
  :after dired
  :custom
  (map! :map dired-mode-map
        :n "TAB" #'dirvish-toggle-subtree
        :n "s" #'dirvish-quicksort
        :n "/" #'dirvish-filter)

  (setq dirvish-bookmark-entries
        '(("Downloads" "~/Downloads/")
          ("Dotfiles" "~/.config/")
          ("Persist" "/persistent/home/linuxing3/")
          ("Projects" "~/sources/")))

  (dirvish-header-line-format '(:left (path)))
  (dirvish-mode-line-format '(:left (info)))
  (dirvish-preview-dispatchers (list 'image 'pdf))
  :config
  (setq dirvish-mode-line-format
        '(:left (index) :right (details)))
  (setq inhibit-compacting-font-caches t)
  (setq dirvish-attributes '(file-time file-size git))
  (setq dirvish-hide-details t)
  (setq delete-by-moving-to-trash t))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (setq diredfl-light-blue "#5fafd7"))

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (setq dired-hide-dotfiles-verbose nil))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("k" . dired-subtree-remove)))

;; (use-package dired-ranger
;;   :after dired)

(use-package dired-quick-sort
  :hook (dired-mode . dired-quick-sort-setup))

(use-package async
  :config
  (dired-async-mode 1))

;; dired
(map! "C-c j" #'dired-jump)
(map! :map dired-mode-map
      "i" #'dired-display-file ; quick view
      "j" #'dired-next-line
      "k" #'dired-previous-line
      "h" #'dired-up-directory
      "l" #'dired-open-file ; use dired-find-file instead of dired-open.
      "m" #'dired-mark
      "t" #'dired-toggle-marks
      "u" #'dired-unmark
      "C" #'dired-do-copy
      "D" #'dired-do-delete
      "X" #'dired-do-delete
      "J" #'dired-goto-file
      "M" #'dired-do-chmod
      "O" #'dired-do-chown
      "P" #'dired-do-print
      "R" #'dired-do-rename
      "A" #'dired-do-rename
      "T" #'dired-do-touch
      "Y" #'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
      "Z" #'dired-do-compress
      "a" #'dired-create-directory
      "+" #'dired-create-directory
      "-" #'dired-do-kill-lines
      "% l" #'dired-downcase
      "% m" #'dired-mark-files-regexp
      "% u" #'dired-upcase
      "* %" #'dired-mark-files-regexp
      "* ." #'dired-mark-extension
      "* /" #'dired-mark-directories
      "; d" #'epa-dired-do-decrypt
      "; e" #'epa-dired-do-encrypt)
