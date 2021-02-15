;;; init.el -*- lexical-binding: t; -*-


(doom!
       :completion
       (company +childframe)
       (ivy +icons +presscient)

       :ui
       doom
       doom-dashboard
       fill-column       
       hl-todo           
       modeline          
       workspaces
       (ligatures +extra)

       :editor
       (evil +everywhere)
       file-templates
       fold
       multiple-cursors
       format
       snippets
       word-wrap

       :emacs
       (dired +icons)
       electric          
       ibuffer           

       :checkers
       syntax              

       :tools
       (eval +overlay)
       magit
       lsp
       lookup              
       gist
       upload
       

       :lang
       markdown
       (org +pandoc +journal +brain +present +hugo)
       (python +lsp)
       (go +lsp)
       (cc +lsp)
       (java +lsp)
       (javascript +lsp)
       (web +lsp)
       ; (sh +lsp)
       ;; rest
       plantuml
       (rust +lsp)
       (dart +lsp)
       (json +lsp)
       emacs-lisp)
