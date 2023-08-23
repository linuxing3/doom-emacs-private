;;; private/themes/+modeline.el -*- lexical-binding: t; -*-

;;
;; Plugins
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-modeline! xingwenju
  (bar major-mode " - " buffer-info-simple)
  (matches buffer-encoding))

(defun my-set-minimal-modeline ()
  (progn
    (doom-set-modeline 'xingwenju t)
    (+doom-modeline|refresh-bars +doom-modeline-bar-width +doom-modeline-height)
    )
  "Setting the my minimal modeline for current buffer.")

(my-set-minimal-modeline)
;;; End
