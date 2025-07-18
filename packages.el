;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or

(package! org-super-agenda)
(package! exwm)
(package! counsel)
(package! helix :recipe (:host github :repo "mgmarlow/helix-mode"))

