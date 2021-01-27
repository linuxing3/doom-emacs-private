;;; init.el -*- lexical-binding: t; -*-


(doom!
       :completion
       company           ; the ultimate code completion backendne...
       ivy               ; a search engine for love and life

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       workspaces        ; tab emulation, persistence & separate workspaces
       treemacs          ; a project drawer, like neotree but cooler

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format           ; automated prettiness
       snippets          ; my elves. They type so I don't have to

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management

       :term
       ;;eshell            ; the elisp shell that works everywhere

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       (lsp +peek)
       (debugger +lsp)
       (eval +overlay)
       lookup              ; navigate your code and its documentation
       magit
       make
       gist
       upload

       :lang
       emacs-lisp
       markdown
       org
       plantuml
       (cc +lsp)
       (python +lsp)
       (sh +lsp)
       (web +lsp)               ; the tubes
       (javascript +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (go +lsp)
       (java +lsp +meghanada) ; the poster child for carpal tunnel syndrome
       (rust +lsp)
       (org +pandoc +journal +brain +present)
)
