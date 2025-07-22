;; -*- no-byte-compile: t; -*-
;;; private/org/packages.el

(package! org-super-agenda)
(package! org-pomodoro)
(package! org-cliplink)

(when (featurep! +babel)
  (when (featurep! :lang javascript)
    (package! ob-typescript)))

(when (featurep! +export)
  (package! ox-epub))

(when (featurep! +brain)
  (package! org-brain))

(when (featurep! +trello)
  (package! org-trello))
