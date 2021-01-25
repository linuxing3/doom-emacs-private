;; -*- no-byte-compile: t; -*-
;;; private/writer/packages.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using w3m
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package! w3m)

(package! youdao-dictionary)

(when (featurep! :lang org)
  (package! org-ref)
  (package! org-noter))

(package! company-english-helper
  :recipe (:fetcher github
                    :repo "manateelazycat/company-english-helper"
                    :files ("*")))

(package! insert-translated-name
 :recipe (:fetcher github
          :repo "manateelazycat/insert-translated-name"
          :files ("*")))
