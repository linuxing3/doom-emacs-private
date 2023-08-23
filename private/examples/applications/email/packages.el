;; -*- no-byte-compile: t; -*-
;;; app-x/email/packages.el

(when (featurep! +notmuch)
  (package! prodigy)
  (package! org-mime)
  (package! counsel-notmuch)
  (package! notmuch))
