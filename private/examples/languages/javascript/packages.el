;;; private/javascriptpackages.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Javascript development
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep! :lang javascript)
  (package! prettier-js)
  (package! js-doc))

(when (featurep! +indium)
  (package! indium))

(when (featurep! +tern)
  (package! tern)
  (when (featurep! :completion company)
    (package! company-tern)))
