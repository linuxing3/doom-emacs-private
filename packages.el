;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;;; on the command line, then restart Emacs for the changes to take effect -- or
(package! hydra)

(package! org-super-agenda)

(package! exwm)

(package! counsel)

(package! helix :recipe (:host github :repo "mgmarlow/helix-mode"))

(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el aider-doom.el")))

(package! visual-fill-column)

(package! desktop-environment :recipe (:host github :repo "DamienCassou/desktop-environment"))
