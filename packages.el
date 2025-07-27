;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;;; on the command line, then restart Emacs for the changes to take effect -- or
(package! hydra)

(package! org-super-agenda)

(package! exwm)

(package! counsel)

(package! consult-omni :recipe (:host github :repo "armindarvish/consult-omni" :files (:defaults "sources/*.el")))
(package! browser-hist  :recipe (:host github :repo "agzam/browser-hist.el"))

(package! helix :recipe (:host github :repo "mgmarlow/helix-mode"))

(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el aider-doom.el")))

(package! visual-fill-column)

(package! desktop-environment :recipe (:host github :repo "DamienCassou/desktop-environment"))

(package! flycheck-aspell)

(package! calfw)
(package! calfw-org)

(package! dired-open)
(package! dired-subtree)
(package! dirvish)
(package! diredfl)
(package! all-the-icons-dired)
(package! dired-hide-dotfiles)
(package! dired-ranger)
(package! dired-quick-sort)
(package! async)

(package! dmenu)
(package! ednc)
(package! emojify)

(package! org-web-tools)
