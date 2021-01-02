;;; init.el -*- lexical-binding: t; -*-


(doom!
       :completion
       company           ; the ultimate code completion backendne...
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format           ; automated prettiness

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       (eval +overlay)
       magit
       gist

       :lang
       emacs-lisp
       markdown
       (org +pandoc +journal +brain +present)
)
