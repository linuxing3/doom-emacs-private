;;; +hydras.el -*- lexical-binding: t; -*-

(defhydra hydra-yasnippet (:color blue :hint nil)
  "
              ^YASnippets^
--------------------------------------------
  Modes:    Load/Visit:    Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all))

;; (spacemacs/set-leader-keys "os" 'hydra-yasnippet/body)

(defhydra hydra-outline (:color pink :hint nil)
  "
^Hide^             ^Show^           ^Move
^^^^^^------------------------------------------------------
_q_: sublevels     _a_: all         _u_: up
_t_: body          _e_: entry       _n_: next visible
_o_: other         _i_: children    _p_: previous visible
_c_: entry         _k_: branches    _f_: forward same level
_l_: leaves        _s_: subtree     _b_: backward same level
_d_: subtree

"
  ;; Hide
  ("q" hide-sublevels) ; Hide everything but the top-level headings
  ("t" hide-body)	 ; Hide everything but headings (all body lines)
  ("o" hide-other) ; Hide other branches
  ("c" hide-entry) ; Hide this entry's body
  ("l" hide-leaves) ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree) ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)          ; Show (expand) everything
  ("e" show-entry)        ; Show this heading's body
  ("i" show-children)	; Show this heading's immediate child sub-headings
  ("k" show-branches)	; Show all sub-headings under this heading
  ("s" show-subtree) ; Show (expand) everything in this heading & below
  ;; Move
  ("u" outline-up-heading)			   ; Up
  ("n" outline-next-visible-heading)	   ; Next
  ("p" outline-previous-visible-heading) ; Previous
  ("f" outline-forward-same-level)	   ; Forward - same level
  ("b" outline-backward-same-level)	   ; Backward - same level
  ("z" nil "leave"))

;; (global-set-key (kbd "C-c #") 'hydra-outline/body) ; by example

(defhydra hydra-info (:color pink
                             :hint nil)
  "
Info-mode:
_I_ndex(virtual)    _T_OC                            ^ ^^ ^  ^ ^ ^^     _k_/_u_p   ( )
_i_ndex             _t_op node        Node           _[__h_ + _l__]_      _j_/_m_enu ( ) (C-u for new window)
_c_opy node name    _a_propos         Top/Final Node _<__t_   ^ ^_>_      _g_oto node^^    (C-u for new window)
_C_lone buffer      _f_ollow          Level nxt/prev _p_^ ^   ^ ^_n_
_d_irectory         _b_mkp-jump       History        _H_^ ^   ^ ^_L_      _K_ History^^

_s_earch regex (_S_ case sens) ^^^^   _1_ .. _9_ Pick first .. ninth item in the node's menu.
"
  ("j"   Info-menu) ;; m
  ("k"   Info-up)	  ;; ^
  ("m"   Info-menu)
  ("u"   Info-up)

  ("l"   Info-forward-node)
  ("h"   Info-backward-node)
  ("]"   Info-forward-node)
  ("["   Info-backward-node)

  ("t"   Info-top-node)
  ("<"   Info-top-node)
  (">"   Info-final-node)

  ("n"   Info-next)
  ("p"   Info-prev)

  ("K"   Info-history)
  ("H"   Info-history-back)
  ("L"   Info-history-forward)

  ("s"   Info-search)
  ("S"   Info-search-case-sensitively)

  ("g"   Info-goto-node)

  ("f"   Info-follow-reference)
  ("b"   bmkp-info-jump)
  ("i"   Info-index)
  (","   Info-index-next)
  ("I"   Info-virtual-index)

  ("T"   Info-toc)
  ("t"   Info-top-node)
  ("d"   Info-directory)
  ("c"   Info-copy-current-node-name)
  ("C"   clone-buffer)
  ("a"   info-apropos)

  ("1"   Info-nth-menu-item)
  ("2"   Info-nth-menu-item)
  ("3"   Info-nth-menu-item)
  ("4"   Info-nth-menu-item)
  ("5"   Info-nth-menu-item)
  ("6"   Info-nth-menu-item)
  ("7"   Info-nth-menu-item)
  ("8"   Info-nth-menu-item)
  ("9"   Info-nth-menu-item)

  ("?"   Info-summary "Info summary")
  ("y"   Info-help "Info help")
  ("q"   Info-exit "Info exit" :color blue)
  ("C-g" nil "cancel" :color blue))

;; (define-key Info-mode-map (kbd ".") #'hydra-info/body)

(defhydra hydra-macro (:hint nil :color pink :pre
                             (when defining-kbd-macro
                               (kmacro-end-macro 1)))
  "
  ^Create-Cycle^   ^Basic^           ^Insert^        ^Save^         ^Edit^
╭─────────────────────────────────────────────────────────────────────────╯
     ^_i_^           [_e_] execute    [_n_] insert    [_b_] name      [_'_] previous
     ^^↑^^           [_d_] delete     [_t_] set       [_K_] key       [_,_] last
 _j_ ←   → _l_       [_o_] edit       [_a_] add       [_x_] register
     ^^↓^^           [_r_] region     [_f_] format    [_B_] defun
     ^_k_^           [_m_] step
    ^^   ^^          [_s_] swap
"
  ("j" kmacro-start-macro :color blue)
  ("l" kmacro-end-or-call-macro-repeat)
  ("i" kmacro-cycle-ring-previous)
  ("k" kmacro-cycle-ring-next)
  ("r" apply-macro-to-region-lines)
  ("d" kmacro-delete-ring-head)
  ("e" kmacro-end-or-call-macro-repeat)
  ("o" kmacro-edit-macro-repeat)
  ("m" kmacro-step-edit-macro)
  ("s" kmacro-swap-ring)
  ("n" kmacro-insert-counter)
  ("t" kmacro-set-counter)
  ("a" kmacro-add-counter)
  ("f" kmacro-set-format)
  ("b" kmacro-name-last-macro)
  ("K" kmacro-bind-to-key)
  ("B" insert-kbd-macro)
  ("x" kmacro-to-register)
  ("'" kmacro-edit-macro)
  ("," edit-kbd-macro)
  ("q" nil :color blue))



(defhydra hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise))

(defhydra hydra-ag (:exit t
                          :columns 2
                          :idle 1.0)
  "Ag Search"
  ("c" helm-ag "Current directory")
  ("d" (lambda ()
         (interactive)
         (let ((current-prefix-arg '(4)))
           (call-interactively 'helm-ag))) "Select directory")
  ("D" helm-do-ag "Select directory (interactive)")
  ("f" helm-ag-this-file "Current file")
  ("F" helm-do-ag-this-file "Current file (interactive)")
  ("p" helm-ag-project-root "Project")
  ("b" helm-ag-buffers "Buffers")
  ("B" helm-do-ag-buffers "Buffers (interactive)"))

;; (spacemacs/set-leader-keys "s." 'hydra-ag/body)

;; Hydra - Multiple cursors
(defhydra multiple-cursors-hydra (:columns 3
                                           :idle 1.0)
  "Multiple cursors"
  ("l" mc/edit-lines "Edit lines in region" :exit t)
  ("b" mc/edit-beginnings-of-lines "Edit beginnings of lines in region" :exit t)
  ("e" mc/edit-ends-of-lines "Edit ends of lines in region" :exit t)
  ("a" mc/mark-all-dwim "Mark all dwim" :exit t)
  ("S" mc/mark-all-symbols-like-this "Mark all symbols likes this" :exit t)
  ("w" mc/mark-all-words-like-this "Mark all words like this" :exit t)
  ("r" mc/mark-all-in-region "Mark all in region" :exit t)
  ("R" mc/mark-all-in-region-regexp "Mark all in region (regexp)" :exit t)
  ("d" mc/mark-all-like-this-in-defun "Mark all like this in defun" :exit t)
  ("s" mc/mark-all-symbols-like-this-in-defun "Mark all symbols like this in defun" :exit t)
  ("W" mc/mark-all-words-like-this-in-defun "Mark all words like this in defun" :exit t)
  ("i" mc/insert-numbers "Insert numbers" :exit t)
  ("n" mc/mark-next-like-this "Mark next like this")
  ("N" mc/skip-to-next-like-this "Skip to next like this")
  ("M-n" mc/unmark-next-like-this "Unmark next like this")
  ("p" mc/mark-previous-like-this "Mark previous like this")
  ("P" mc/skip-to-previous-like-this "Skip to previous like this")
  ("M-p" mc/unmark-previous-like-this "Unmark previous like this")
  ("q" nil "Quit" :exit t))

(after! org
  (add-to-list 'org-structure-template-alist
               '("C" "#+BEGIN_COMMENT\n?\n#+END_COMMENT" "")))
;; (spacemacs/set-leader-keys "mm" 'multiple-cursors-hydra/body)
;;;###autoload(autoload 'hydra-org-template/body "private/personal/autoload/+hydras" nil t)
(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote    plant_u_ml    _L_aTeX:
_l_atex   _e_xample  _s_hell       _i_ndex:
_a_scii   _v_erse    _E_macs-lisp  _I_NCLUDE:
s_r_c     _C_omment  _p_ython      _H_TML:
_h_tml    ^ ^        Lil_y_pond    _A_SCII:
"
  ("c" (hot-expand-and-edit "comment"))
  ("s" (hot-expand-and-edit "shell"))
  ("E" (hot-expand-and-edit "emacs-lisp"))
  ("p" (hot-expand-and-edit "python"))
  ("y" (hot-expand-and-edit "lilypond"))
  ("u" (hot-expand-and-edit "plantuml :file CHANGE.png"))
  ;; ("r" (hot-expand "<s"))
  ("r" (call-interactively 'src-expand-and-edit))
  ("C" (hot-expand "<C"))
  ("e" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))
(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))
(defun hot-expand-and-edit (str)
  "Expand src template for given languange and enter org-edit-special."
  (hot-expand "<s")
  (insert str)
  (forward-line)
  (evil-normal-state)
  (org-edit-special)
  (evil-insert-state))
(defun src-expand-and-edit ()
  "Expand select src type block and edit in org-mode"
  (interactive)
  (ivy-read "Source code type: "
            '("emacs-lisp" "python" "C" "shell" "java" "js" "clojure" "C++" "css"
              "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
              "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
              "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
              "scheme" "sqlite")
            :action (lambda (x) (hot-expand-and-edit x ))
            ))

(defhydra sk/hydra-org-agenda-view (:color red
                                    :hint nil)
  "
 _d_: day        _g_: time grid    _a_: arch-trees    _L_: log closed clock
 _w_: week       _i_: inactive     _A_: arch-files    _c_: log clock check
 _t_: fortnight  _f_: follow       _r_: report        _l_: log mode toggle
 _m_: month      _e_: entry        _D_: diary         _q_: quit
 _y_: year       _!_: deadlines    _R_: reset
"
  ("R" org-agenda-reset-view)
  ("d" org-agenda-day-view)
  ("w" org-agenda-week-view)
  ("t" org-agenda-fortnight-view)
  ("m" org-agenda-month-view)
  ("y" org-agenda-year-view)
  ("l" org-agenda-log-mode)
  ("L" (org-agenda-log-mode '(4)))
  ("c" (org-agenda-log-mode 'clockcheck))
  ("f" org-agenda-follow-mode)
  ("a" org-agenda-archives-mode)
  ("A" (org-agenda-archives-mode 'files))
  ("r" org-agenda-clockreport-mode)
  ("e" org-agenda-entry-text-mode)
  ("g" org-agenda-toggle-time-grid)
  ("D" org-agenda-toggle-diary)
  ("!" org-agenda-toggle-deadlines)
  ("i"
   (let ((org-agenda-include-inactive-timestamps t))
     (org-agenda-check-type t 'timeline 'agenda)
     (org-agenda-redo)))
  ("q" nil :color blue))

;;;###autoload(autoload 'hydra-org-agenda/body "private/personal/autoload/+hydras" nil t)
;; Hydra for org agenda (graciously taken from Spacemacs)
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

(defhydra hydra-dired (:hint nil :color pink)
  "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
  ("\\" dired-do-ispell)
  ("(" dired-hide-details-mode)
  (")" dired-omit-mode)
  ("+" dired-create-directory)
  ("=" diredp-ediff) ;; smart diff
  ("?" dired-summary)
  ("$" diredp-hide-subdir-nomove)
  ("A" dired-do-find-regexp)
  ("C" dired-do-copy)	;; Copy all marked files
  ("D" dired-do-delete)
  ("E" dired-mark-extension)
  ("e" dired-ediff-files)
  ("F" dired-do-find-marked-files)
  ("G" dired-do-chgrp)
  ("g" revert-buffer)	;; read all directories again (refresh)
  ("i" dired-maybe-insert-subdir)
  ("l" dired-do-redisplay) ;; relist the marked or singel directory
  ("M" dired-do-chmod)
  ("m" dired-mark)
  ("O" dired-display-file)
  ("o" dired-find-file-other-window)
  ("Q" dired-do-find-regexp-and-replace)
  ("R" dired-do-rename)
  ("r" dired-do-rsynch)
  ("S" dired-do-symlink)
  ("s" dired-sort-toggle-or-edit)
  ("t" dired-toggle-marks)
  ("U" dired-unmark-all-marks)
  ("u" dired-unmark)
  ("v" dired-view-file) ;; q to exit, s to search, = gets line #
  ("w" dired-kill-subdir)
  ("Y" dired-do-relsymlink)
  ("z" diredp-compress-this-file)
  ("Z" dired-do-compress)
  ("q" nil)
  ("." nil :color blue))

(defhydra alt_s_hydras_menu (:columns 2 :exit t)
  "M-s menu"

  ("." isearch-forward-symbol-at-point "Isearch symbol at point")
  ("_" isearch-forward-symbol "Do incremental search forward for a symbol")
  ("o" occur "Show occurencies")
  ("f" copy-current-buffer-name "Remember current buffer name")
  ("w" isearch-forward-word "Isearch forward word")
  ("h." highlight-symbol-at-point "Highlight symbol at point")
  ("hl" highlight-lines-matching-regexp "Highlight lines matcing RegExp")
  ("hp" highlight-phrase "Highlight phrase")
  ("hr" highlight-regexp "Highlight RegExp")
  ("hu" unhighlight-regexp "Unhighlight RegExp")
  ("hw" hi-lock-write-interactive-patterns "Write interactive patterns")
  ("M-w" eww-search-words "Search the web for the text")
  )
;; from http://kitchingroup.cheme.cmu.edu/blog/category/hydra/
;;;###autoload(autoload 'goto/body "private/personal/autoload/+hydras" nil t)
(defhydra goto (:color blue :hint nil)
  "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
  ("c" avy-goto-char-2)
  ("C" avy-goto-char)
  ("L" avy-goto-char-in-line)
  ("w" avy-goto-word-1)
  ;; jump to beginning of some word
  ("W" avy-goto-word-0)
  ;; jump to subword starting with a char
  ("s" avy-goto-subword-1)
  ;; jump to some subword
  ("S" avy-goto-subword-0)

  ("l" avy-goto-line)
  ("i" ace-window)

  ("h" helm-org-headlines)
  ("a" helm-org-agenda-files-headings)
  ("q" helm-multi-swoop-org)

  ("o" helm-occur)
  ("p" swiper-helm)

  ("f" isearch-forward)
  ("b" isearch-backward)

  ("." org-mark-ring-push :color red)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("m" helm-mini)
  ("R" helm-recentf)
  ("n" hydra-navigate/body))

;;;###autoload(autoload 'hydra-navigate/body "private/personal/autoload/+hydras" nil t)
(defhydra hydra-navigate (:color red
                          :hint nil)
  "
_f_: forward-char       _w_: forward-word       _n_: next-line
_b_: backward-char      _W_: backward-word      _p_: previous-line
^ ^                     _o_: subword-right      _,_: beginning-of-line
^ ^                     _O_: subword-left       _._: end-of-line

_s_: forward sentence   _a_: forward paragraph  _g_: forward page
_S_: backward sentence  _A_: backward paragraph _G_: backward page

_h_: helm mini _B_: buffer list _i_: window
_<left>_: previous buffer   _<right>_: next buffer
_<up>_: scroll-up           _<down>_: scroll-down

_[_: backward-sexp _]_: forward-sexp
_<_ beginning of buffer _>_ end of buffer _m_: set mark _/_: jump to mark
"
  ("f" forward-char)
  ("b" backward-char)
  ("w" forward-word)
  ("W" backward-word)
  ("n" next-line)
  ("p" previous-line)
  ("o" subword-right)
  ("O" subword-left)
  ("s" forward-sentence)
  ("S" backward-sentence)
  ("a" forward-paragraph)
  ("A" backward-paragraph)
  ("g" forward-page)
  ("G" backward-page)
  ("<right>" next-buffer)
  ("<left>" previous-buffer)
  ("h" helm-mini :color blue)
  ("i" ace-window :color blue)
  ("m" org-mark-ring-push)
  ("/" org-mark-ring-goto :color blue)
  ("B" helm-buffers-list)
  ("<up>" scroll-up)
  ("<down>" scroll-down)
  ("<" beginning-of-buffer)
  (">" end-of-buffer)
  ("." end-of-line)
  ("[" backward-sexp)
  ("]" forward-sexp)
  ("," beginning-of-line)
  ("q" nil "quit" :color blue))

;;;###autoload(autoload 'hydra-yank-pop/body "$HOME/.doom.d/modules/private/personal/autoload/+hydras" nil t)
;;;###autoload(autoload 'hydra-yank-pop/evil-paste-after"$HOME/.doom.d/modules/private/personal/autoload/+hydras" nil t)
;;;###autoload(autoload 'hydra-yank-pop/evil-paste-before"$HOME/.doom.d/modules/private/personal/autoload/+hydras" nil t)
(defhydra hydra-yank-pop ()
  "yank"
  ("C-j" evil-paste-pop "next")
  ("C-k" evil-paste-pop-next "prev")
  ("p" evil-paste-after nil)
  ("P" evil-paste-before nil)
  ("C-l" counsel-yank-pop "list" :color blue))   ; or browse-kill-ring

;; (global-set-key (kbd "M-y") #'hydra-yank-pop/yank-pop)
;; (global-set-key (kbd "C-y") #'hydra-yank-pop/yank)

(defhydra sk/hydra-of-windows (:color red
                               :hint nil)
  "
 ^Move^    ^Size^    ^Change^                    ^Split^           ^Text^
 ^^^^^^^^^^^------------------------------------------------------------------
 ^ ^ _k_ ^ ^   ^ ^ _K_ ^ ^   _u_: winner-undo _o_: rotate  _v_: vertical     _+_: zoom in
 _h_ ^+^ _l_   _H_ ^+^ _L_   _r_: winner-redo            _s_: horizontal   _-_: zoom out
 ^ ^ _j_ ^ ^   ^ ^ _J_ ^ ^   _c_: close                  _z_: zoom         _q_: quit
"
  ("h" windmove-left)
  ("j" windmove-down)
  ("k" windmove-up)
  ("l" windmove-right)
  ("H" shrink-window-horizontally)
  ("K" shrink-window)
  ("J" enlarge-window)
  ("L" enlarge-window-horizontally)
  ("v" evil-window-vsplit)
  ("s" evil-window-split)
  ("c" delete-window)
  ("f" toggle-win :color blue)
  ("o" evil-window-rotate-downwards)
  ("z" doom/window-zoom)
  ("u" (progn
         (winner-undo)
         (setq this-command 'winner-undo)))
  ("r" winner-redo)
  ("+" text-scale-increase)
  ("-" text-scale-decrease)
  ("q" nil :color blue))

(defhydra sk/hydra-multiple-cursors (:color red
                                     :hint nil)
  "
  _k_: prev         _j_: next         _a_: all     _b_: beg of lines   _q_: quit
  _K_: skip prev    _J_: skip next    _d_: defun   _e_: end of lines
  _p_: unmark prev  _n_: unmark next  _r_: regexp  _l_: lines
"
  ("j" mc/mark-next-like-this)
  ("J" mc/skip-to-next-like-this)
  ("n" mc/unmark-next-like-this)
  ("k" mc/mark-previous-like-this)
  ("K" mc/skip-to-previous-like-this)
  ("p" mc/unmark-previous-like-this)
  ("a" mc/mark-all-like-this :color blue)
  ("d" mc/mark-all-like-this-in-defun :color blue)
  ("r" mc/mark-all-in-region-regexp :color blue)
  ("b" mc/edit-beginnings-of-lines)
  ("e" mc/edit-ends-of-lines)
  ("l" mc/edit-lines :color blue)
  ("q" nil :color blue))

(defhydra sk/hydra-rectangle (:pre (rectangle-mark-mode 1)
                              :color pink
                              :hint nil)
  "
 _p_: paste   _r_: replace  _I_: insert
 _y_: copy    _o_: open     _V_: reset
 _d_: kill    _n_: number   _q_: quit
"
  ("h" backward-char nil)
  ("l" forward-char nil)
  ("k" previous-line nil)
  ("j" next-line nil)
  ("y" copy-rectangle-as-kill)
  ("d" kill-rectangle)
  ("x" clear-rectangle)
  ("o" open-rectangle)
  ("p" yank-rectangle)
  ("r" string-rectangle)
  ("n" rectangle-number-lines)
  ("I" string-insert-rectangle)
  ("V" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)) nil)
  ("q" keyboard-quit :color blue))

(defhydra sk/hydra-registers (:color blue
                              :hint nil)
  "
 _a_: append     _c_: copy-to    _j_: jump       _r_: rectangle-copy   _q_: quit
 _i_: insert     _n_: number-to  _f_: frameset   _w_: window-config
 _+_: increment  _p_: point-to
  "
  ("a" append-to-register)
  ("c" copy-to-register)
  ("i" insert-register)
  ("f" frameset-to-register)
  ("j" jump-to-register)
  ("n" number-to-register)
  ("r" copy-rectangle-to-register)
  ("w" window-configuration-to-register)
  ("+" increment-register)
  ("p" point-to-register)
  ("q" nil :color blue))

(defhydra sk/hydra-of-activate (:color blue
                                :hint nil)
  "
 _b_: battery   _n_: number       _v_: wrap        _c_: column    _i_: indent        _k_: which-key   _l_: talk       _q_: quit
 _t_: time      _w_: weather      _y_: yasnippet   _m_: margin    _s_: smartparens   _o_: org extras  _j_: jabber
 _f_: flyspell  _a_: auto-comp    _d_: fold        _g_: ggtags    _p_: paradox       _e_: error       _h_: html emmet
"
  ("b" fancy-battery-mode :color red)
  ("t" display-time-mode :color red)
  ("n" linum-mode :color red)
  ("w" wttrin)
  ("f" flyspell-mode)
  ("v" visual-line-mode)
  ("p" list-packages)
  ("c" column-enforce-mode)
  ("y" yas-global-mode)
  ("a" company-mode)
  ("i" highlight-indentation-mode)
  ("m" fci-mode :color red)
  ("j" jabber-connect :color red)
  ("l" jabber-chat-with)
  ("o" sk/org-custom-load :color blue)
  ("g" ggtags-mode)
  ("d" global-origami-mode)
  ("k" which-key-mode)
  ("s" smartparens-strict-mode)
  ("h" emmet-mode)
  ("e" global-flycheck-mode)
  ("q" nil :color blue))


(defhydra sk/hydra-vimish-fold (:color red
                                :hint nil)
  "
 _f_: fold  _u_: unfold  _r_: refold  _t_: toggle  _d_: delete    _n_: next      _q_: quit
          _U_: Unfold  _R_: Refold  _T_: Toggle  _D_: Delete    _p_: previous
  "
  ("f" vimish-fold)
  ("u" vimish-fold-unfold)
  ("r" vimish-fold-refold)
  ("t" vimish-fold-toggle)
  ("d" vimish-fold-delete)
  ("U" vimish-fold-unfold-all)
  ("R" vimish-fold-refold-all)
  ("T" vimish-fold-toggle-all)
  ("D" vimish-fold-delete-all)
  ("n" vimish-fold-next-fold)
  ("p" vimish-fold-previous-fold)
  ("q" nil :color blue))

(defhydra hydra-ag (:color blue :hint nil)
                "
silversearcher: ag(1)

 Files                  Directories
------------------------------------------
 _<f8>_ ag-files          _8_ ag-project-dired
 _<f9>_ ag                _9_ ag-dired
_<f10>_ ag-regexp         _0_ ag-dired-regexp
"
                ("<f8>" ag-files "ag-files")
                ("<f9>" ag "ag")
                ("<f10>" ag-regexp "ag-regexp")
                ("8" ag-project-dired "ag-project-dired")
                ("9" ag-dired "ag-dired")
                ("0" ag-dired-regexp "ag-dired-regexp"))


(defun +xfu/set--transparency (inc)
  "Increase or decrease the selected frame transparency"
  (let* ((alpha (frame-parameter (selected-frame) 'alpha))
         (next-alpha (cond ((not alpha) 100)
                           ((> (- alpha inc) 100) 100)
                           ((< (- alpha inc) 0) 0)
                           (t (- alpha inc)))))
    (set-frame-parameter (selected-frame) 'alpha next-alpha)))

(defhydra +xfu/set-transparency (:columns 2)
  "
ALPHA : [ %(frame-parameter nil 'alpha) ]
"
  ("j" (lambda () (interactive) (+xfu/set--transparency +1)) "+ more")
  ("k" (lambda () (interactive) (+xfu/set--transparency -1)) "- less")
  ("J" (lambda () (interactive) (+xfu/set--transparency +10)) "++ more")
  ("K" (lambda () (interactive) (+xfu/set--transparency -10)) "-- less")
  ("=" (lambda (value) (interactive "nTransparency Value 0 - 100 opaque:")
         (set-frame-parameter (selected-frame) 'alpha value)) "Set to ?" :color blue))

;;;###autoload(autoload 'hydra/timestamp/body "$HOME/.doom.d/modules/private/personal/autoload/+hydras" nil t)
(defhydra hydra/timestamp (:color blue :hint nil)
  "
Timestamps: (_q_uit)
  Date: _I_SO, _U_S, US With _Y_ear and _D_ashes, US In _W_ords
   Date/Time: _N_o Colons or _w_ith
    Org-Mode: _R_ight Now or _c_hoose
"
  ("q" nil)

  ("I" help/insert-datestamp)
  ("U" help/insert-datestamp-us)
  ("Y" help/insert-datestamp-us-full-year)
  ("D" help/insert-datestamp-us-full-year-and-dashes)
  ("W" help/insert-datestamp-us-words)

  ("N" help/insert-timestamp-no-colons)
  ("w" help/insert-timestamp)

  ("R" help/org-time-stamp-with-seconds-now)
  ("c" org-time-stamp))

;;;###autoload
(defun help/insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))
;;;###autoload
(defun help/insert-datestamp-us ()
  "Produces and inserts a US datestamp."
  (interactive)
  (insert (format-time-string "%m/%d/%y")))
;;;###autoload
(defun help/insert-datestamp-us-full-year-and-dashes ()
  "Produces and inserts a US datestamp with full year and dashes."
  (interactive)
  (insert (format-time-string "%m-%d-%Y")))
;;;###autoload
(defun help/insert-datestamp-us-full-year ()
  "Produces and inserts a US datestamp with full year."
  (interactive)
  (insert (format-time-string "%m/%d/%Y")))
;;;###autoload
(defun help/insert-datestamp-us-words ()
  "Produces and inserts a US datestamp using words."
  (interactive)
  (insert (format-time-string "%A %B %d, %Y")))
;;;###autoload
(defun help/insert-timestamp-no-colons ()
  "Inserts a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (insert (help/get-timestamp-no-colons)))
;;;###autoload
(defun help/insert-datestamp ()
  "Produces and inserts a partial ISO 8601 format timestamp."
  (interactive)
  (insert (format-time-string "%F")))
;;;###autoload
(defun help/get-timestamp-no-colons ()
  "Produces a full ISO 8601 format timestamp with colons replaced by hyphens."
  (interactive)
  (let* ((timestamp (help/get-timestamp))
         (timestamp-no-colons (replace-regexp-in-string ":" "-" timestamp)))
    timestamp-no-colons))
;;;###autoload
(defun help/get-timestamp ()
  "Produces a full ISO 8601 format timestamp."
  (interactive)
  (let* ((timestamp-without-timezone (format-time-string "%Y-%m-%dT%T"))
         (timezone-name-in-numeric-form (format-time-string "%z"))
         (timezone-utf-offset
          (concat (substring timezone-name-in-numeric-form 0 3)
                  ":"
                  (substring timezone-name-in-numeric-form 3 5)))
         (timestamp (concat timestamp-without-timezone
                            timezone-utf-offset)))
    timestamp))
;;;###autoload
(defun help/insert-timestamp ()
  "Inserts a full ISO 8601 format timestamp."
  (interactive)
  (insert (help/get-timestamp)))
;;;###autoload
(defun help/org-time-stamp-with-seconds-now ()
  (interactive)
  (let ((current-prefix-arg '(16)))
    (call-interactively 'org-time-stamp)))
