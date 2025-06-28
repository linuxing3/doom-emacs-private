;; -*- no-byte-compile: t; -*-
;;; feature-x/chat/packages.el

(defconst slack-packages
  '(
	alert
    emoji-cheat-sheet-plus
    persp-mode
    slack
    ))

(dolist (pkg slack-packages)
  (package! pkg))
