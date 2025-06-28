;;; lang-x/org/config.el -*- lexical-binding: t; -*-

(after! org
  (setq
   org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
   org-ellipsis " ▼ "
   )
  ;; from https://emacs.stackexchange.com/questions/30520/org-mode-c-c-c-c-to-display-inline-image
  ;; TODO only redisplay affect source block
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

  )

;; TODO: create org-brain workspace for all brain files
;; create local brain lib
(def-package! org-brain
  :commands org-brain-visualize
  :init
  (setq org-brain-path "~/org/brain")
  ;; (add-hook 'org-agenda-mode-hook #'(lambda () (evil-vimish-fold-mode -1)))
  (set! :evil-state 'org-brain-visualize-mode 'normal)

  :config
  (require 'org)
  (defun org-brain-set-tags (entry)
    "Use `org-set-tags' on headline ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (when (org-brain-filep entry)
      (error "Can only set tags on headline entries"))
    (org-with-point-at (org-brain-entry-marker entry)
      (counsel-org-tag)
      (save-buffer))
    (org-brain--revert-if-visualizing))

  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'root
        org-brain-title-max-length 20)
  (set! :popup "^\\*org-brain\\*$" '((vslot . -1) (size . 0.3) (side . left)) '((select . t) (quit) (transient)))

  (map!
   (:map org-brain-visualize-mode-map
     :n "a"   #'org-brain-visualize-attach
     :n "b"   #'org-brain-visualize-back
     :n "c"   #'org-brain-add-child
     :n "C"   #'org-brain-remove-child
     :n "p"   #'org-brain-add-parent
     :n "P"   #'org-brain-remove-parent
     :n "f"   #'org-brain-add-friendship
     :n "F"   #'org-brain-remove-friendship
     :n "d"   #'org-brain-delete-entry
     :n "^"   #'revert-buffer
     :n "_"   #'org-brain-new-child
     :n ";"   #'org-brain-set-tags
     :n "j"   #'forward-button
     :n "k"   #'backward-button
     :n "l"   #'org-brain-add-resource
     :n "L"   #'doom/org-brain-add-resource
     :n "t"   #'org-brain-set-title
     :n "$"   #'org-brain-pin
     :n "o"   #'ace-link-woman
     :n "q"   #'org-brain-visualize-quit
     :n "r"   #'org-brain-visualize-random
     :n "R"   #'org-brain-visualize-wander
     :n "g"   #'org-brain-visualize
     :n "G"   #'org-brain-goto
     :n [tab] #'org-brain-goto-current
     :n "m"   #'org-brain-visualize-mind-map
     :n "["   #'org-brain-visualize-add-grandchild
     :n "]"   #'org-brain-visualize-remove-grandchild
     :n "{"   #'org-brain-visualize-add-grandparent
     :n "}"   #'org-brain-visualize-remove-grandparent
     )))

(def-package! org-web-tools
  :after org)

(after! org-bullets
  ;; The standard unicode characters are usually misaligned depending on the
  ;; font. This bugs me. Personally, markdown #-marks for headlines are more
  ;; elegant, so we use those.

  (setq org-bullets-bullet-list '("⊢" "⋮" "⋱" " ")))

(def-package! org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("⚡" "⬆" "☕")))

;; Bootstrap
;;
;; (remove-hook! 'org-load-hook #'+org|setup-evil)

(add-hook 'org-load-hook #'+org-private|setup-ui t)
(add-hook 'org-load-hook #'+org-private|setup-overrides t)

(remove-hook! 'org-mode-hook #'(visual-line-mode))

(add-hook 'org-mode-hook #'+org-private|setup-editing t)

;; `org-load' hooks
;;

(defun +org-private|setup-ui ()
  "Configures the UI for `org-mode'."
  ;; (defface org-todo-keyword-todo '((t ())) "org-todo" :group 'org)
  ;; (defface org-todo-keyword-kill '((t ())) "org-kill" :group 'org)
  ;; (defface org-todo-keyword-outd '((t ())) "org-outd" :group 'org)
  ;; (defface org-todo-keyword-wait '((t ())) "org-wait" :group 'org)
  ;; (defface org-todo-keyword-done '((t ())) "org-done" :group 'org)
  ;; (defface org-todo-keyword-habt '((t ())) "org-habt" :group 'org)

  (set! :popup "^\\*Org Src" '((size . 0.4) (side . right)) '((quit) (select . t) (modeline)))
  (set! :popup "^CAPTURE.*\\.org$" '((side . bottom) (size . 0.4)) '((quit) (select . t)))

  (setq org-adapt-indentation nil
        org-export-babel-evaluate nil
        org-blank-before-new-entry nil
        org-clock-clocktable-default-properties (quote (:maxlevel 3 :scope agenda :tags "-COMMENT"))
        org-clock-persist t
        org-clock-persist-file (expand-file-name ".org-clock-persist-data.el" +org-dir)
        org-clocktable-defaults (quote (:maxlevel 3 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 t :fileskip0 t :tags "-COMMENT" :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))
        org-columns-default-format "%50ITEM(Task) %8CLOCKSUM %16TIMESTAMP_IA"
        org-complete-tags-always-offer-all-agenda-tags t
        org-cycle-include-plain-lists t
        org-cycle-separator-lines 1
        org-mac-Skim-highlight-selection-p t
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-global-properties (quote (("Effort_ALL" . "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 7:00")))
        org-hidden-keywords nil
        org-hide-block-startup t
        org-hide-emphasis-markers nil
        org-hide-leading-stars nil
        org-hide-leading-stars-before-indent-mode nil
        org-highest-priority ?A
        org-insert-heading-respect-content t
        org-id-link-to-org-use-id t
        org-id-locations-file (concat +org-dir ".org-id-locations")
        org-id-track-globally t
        org-image-actual-width nil
        org-imenu-depth 8
        org-indent-indentation-per-level 2
        org-indent-mode-turns-on-hiding-stars t
        org-list-description-max-indent 4
        org-log-done 'time
        org-log-into-drawer t
        org-log-note-clock-out t

        org-log-redeadline 'time
        org-log-reschedule 'time
        org-log-state-notes-into-drawer t
        org-lowest-priority ?F
        org-modules (quote (org-bibtex org-habit org-info org-protocol org-mac-link org-notmuch))
        org-outline-path-complete-in-steps nil
        org-pretty-entities nil

        org-pretty-entities-include-sub-superscripts t
        ;; org-priority-faces
        ;; `((?a . ,(face-foreground 'error))
        ;;   (?b . ,(face-foreground 'warning))
        ;;   (?c . ,(face-foreground 'success)))
        org-publish-timestamp-directory (concat +org-dir ".org-timestamps/")
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path 'file
        org-startup-folded t
        org-startup-indented t
        org-startup-with-inline-images nil
        org-tags-column 0
        ;; org-todo-keyword-faces
        ;; '(("TODO" . org-todo-keyword-todo)
        ;;   ("HABT" . org-todo-keyword-habt)
        ;;   ("DONE" . org-todo-keyword-done)
        ;;   ("WAIT" . org-todo-keyword-wait)
        ;;   ("KILL" . org-todo-keyword-kill)
        ;;   ("OUTD" . org-todo-keyword-outd))
        ;; org-todo-keywords
        ;; '((sequence "TODO(t!)"  "|" "DONE(d!/@)")
        ;;   (sequence "WAIT(w@/@)" "|" "OUTD(o@/@)" "KILL(k@/@)")
        ;;   (sequence "HABT(h!)" "|" "DONE(d!/@)" "KILL(k@/@)"))
        org-treat-insert-todo-heading-as-state-change t
        org-use-fast-tag-selection nil
        org-use-fast-todo-selection t
        org-use-sub-superscripts '{}
        outline-blank-line t)

  ;; Update UI when theme is changed
  (add-hook 'doom-init-theme-hook #'+org-private|setup-ui))

(defun +org-private|setup-overrides ()
  (after! org-html
    (defun +org-private/org-html--tags (tags info)
      "Format TAGS into HTML.
INFO is a plist containing export options."
      (when tags
        (format "\n<span class=\"tag\">%s</span>\n"
                (mapconcat
                 (lambda (tag)
                   (format "<span class=\"%s\">%s</span>"
                           (concat (plist-get info :html-tag-class-prefix)
                                   (org-html-fix-class-name tag))
                           tag))
                 tags " "))))
    (advice-add 'org-html--tags :override #'+org-private/org-html--tags))
  (setq org-file-apps
        `(("pdf" . default)
          ("\\.x?html?\\'" . default)
          (auto-mode . emacs)
          (directory . emacs)
          (t . ,(cond (IS-MAC "open \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org/insert-item-with-ts ()
    "When on org timestamp item insert org timestamp item with current time.
This holds only for inactive timestamps."
    (interactive)
    (when (save-excursion
            (let ((item-pos (org-in-item-p)))
              (when item-pos
                (goto-char item-pos)
                (org-list-at-regexp-after-bullet-p org-ts-regexp-inactive))))
      (let ((item-pos (org-in-item-p))
            (pos (point)))
        (assert item-pos)
        (goto-char item-pos)
        (let* ((struct (org-list-struct))
               (prevs (org-list-prevs-alist struct))
               (s (concat (with-temp-buffer
                            (org-insert-time-stamp nil t t)
                            (buffer-string)) " ")))
          (setq struct (org-list-insert-item pos struct prevs nil s))
          (org-list-write-struct struct (org-list-parents-alist struct))
          (looking-at org-list-full-item-re)
          (goto-char (match-end 0))
          (end-of-line)))
      t))

  (defun +org/insert-go-eol ()
    (when (bound-and-true-p evil-mode)
      (evil-insert 1))
    (end-of-line))
  (add-hook 'org-metareturn-hook '+org/insert-item-with-ts)
  (add-hook 'org-metareturn-hook '+org/insert-go-eol)


  (after! elfeed-show
    (map! (:map elfeed-show-mode-map
            :nm "b" #'org-ref-add-bibtex-entry-from-elfeed-entry)))

  (defun +org-private/*org-ctrl-c-ctrl-c-counsel-org-tag ()
    "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
    (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
        (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
            (user-error "C-c C-c can do nothing useful at this location"))
      (let* ((context (org-element-context))
             (type (org-element-type context)))
        (case type
          ;; When at a link, act according to the parent instead.
          (link (setq context (org-element-property :parent context))
                (setq type (org-element-type context)))
          ;; Unsupported object types: refer to the first supported
          ;; element or object containing it.
          ((bold code entity export-snippet inline-babel-call inline-src-block
                 italic latex-fragment line-break macro strike-through subscript
                 superscript underline verbatim)
           (setq context
                 (org-element-lineage
                  context '(radio-target paragraph verse-block table-cell)))))
        ;; For convenience: at the first line of a paragraph on the
        ;; same line as an item, apply function on that item instead.
        (when (eq type 'paragraph)
          (let ((parent (org-element-property :parent context)))
            (when (and (eq (org-element-type parent) 'item)
                       (= (line-beginning-position)
                          (org-element-property :begin parent)))
              (setq context parent type 'item))))
        (case type
          ((headline inlinetask)
           (save-excursion (goto-char (org-element-property :begin context))
                           (call-interactively 'counsel-org-tag)) t)))))
  (add-hook 'org-ctrl-c-ctrl-c-hook '+org-private/*org-ctrl-c-ctrl-c-counsel-org-tag))

;;
;; `org-mode' hooks
;;

(defun +org-private|setup-editing ()
  (after! smartparens
    (sp-with-modes 'org-mode
      (sp-local-pair "\\(" "\\)"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair)
                     )
      (sp-local-pair "\\[" "\\]"
                     :post-handlers '(sp-latex-insert-spaces-inside-pair)
                     ))))
