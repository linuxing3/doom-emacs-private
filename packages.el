;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(unpin! org-mode)
;; (package! org-super-agenda)
(package! org-super-agenda :pin "f5e80e4d0da6b2eeda9ba21e021838fa6a495376")
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! org-roam-server :pin "2093ea5a1a1f2d128dd377778472a481913717b4")

(package! ox-gfm :pin "99f93011b069e02b37c9660b8fcb45dab086a07f")
(package! org-ref :pin "32803203cc4b01e1d4436f0f65138bf569dad8ad")

(package! fzf)

;; (package! xkcd :pin "66e928706fd660cfdab204c98a347b49c4267bdf")

(package! selectric-mode :pin "1840de71f7414b7cd6ce425747c8e26a413233aa")

(package! wttrin :recipe (:local-repo "lisp" :build (:not compile)))

(package! spray :pin "74d9dcfa2e8b38f96a43de9ab0eb13364300cb46")

(package! theme-magic :pin "844c4311bd26ebafd4b6a1d72ddcc65d87f074e3")

(package! elcord :pin "01b26d1af2f33a7c7c5a1c24d8bfb6d40115a7b0")

(package! keycast :pin "a3a0798349adf3e33277091fa8dee63173b68edf")

(package! gif-screencast :pin "1145e676b160e7b1e5756f5b0f30dd31de252e1f")

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")

(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :pin "cc02f2533782d6b9b628cec7e2dcf25b2d05a27c" :disable t)

(package! lexic :recipe (:host github :repo "tecosaur/lexic"))

(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
