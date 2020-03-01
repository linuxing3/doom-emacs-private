;;; c:/Users/Administrator/.doom.d/extensions/feature+magit.el -*- lexical-binding: t; -*-

(defhydra yt-hydra/help (:color blue :hint nil)
  "
_msa_ stage-all _mp_ push _mc_ commit _md_ diff _mla_ log-all _ms_ status
"
  ;;Magit part
  ("msa" magit-stage-modified)
  ("mp" magit-push)
  ("mc" magit-commit)
  ("md" magit-diff)
  ("mla" magit-log-all)
  ("ms" magit-status)
  )

(map! "C-S-G" 'yt-hydra/help/body)
