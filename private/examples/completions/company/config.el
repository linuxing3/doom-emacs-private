;;; completion-x/company/config.el -*- lexical-binding: t; -*-

(def-package! company
  :config
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0.2))

(after! company-box
  (remove-hook 'company-box-selection-hook 'company-box-doc)
  (remove-hook 'company-box-hide-hook 'company-box-doc--hide)
  (setq company-box-icons-yasnippet (all-the-icons-material "short_text" :height 0.7 :face 'all-the-icons-green)
        company-box-icons-unknown (all-the-icons-material "find_in_page" :height 0.7 :face 'all-the-icons-purple)
        company-box-icons-lsp '((1 . (all-the-icons-material "text_fields" :height 0.7 :face 'all-the-icons-green)) ;; Text
                                (2 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Method
                                (3 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Function
                                (4 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Constructor
                                (5 . (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)) ;; Field
                                (6 . (all-the-icons-material "adjust" :height 0.7 :face 'all-the-icons-blue)) ;; Variable
                                (7 . (all-the-icons-material "class" :height 0.7 :face 'all-the-icons-red)) ;; Class
                                (8 . (all-the-icons-material "settings_input_component" :height 0.7 :face 'all-the-icons-red)) ;; Interface
                                (9 . (all-the-icons-material "view_module" :height 0.7 :face 'all-the-icons-red)) ;; Module
                                (10 . (all-the-icons-material "settings" :height 0.7 :face 'all-the-icons-red)) ;; Property
                                (11 . (all-the-icons-material "straighten" :height 0.7 :face 'all-the-icons-red)) ;; Unit
                                (12 . (all-the-icons-material "filter_1" :height 0.7 :face 'all-the-icons-red)) ;; Value
                                (13 . (all-the-icons-material "plus_one" :height 0.7 :face 'all-the-icons-red)) ;; Enum
                                (14 . (all-the-icons-material "filter_center_focus" :height 0.7 :face 'all-the-icons-red)) ;; Keyword
                                (15 . (all-the-icons-material "short_text" :height 0.7 :face 'all-the-icons-red)) ;; Snippet
                                (16 . (all-the-icons-material "color_lens" :height 0.7 :face 'all-the-icons-red)) ;; Color
                                (17 . (all-the-icons-material "insert_drive_file" :height 0.7 :face 'all-the-icons-red)) ;; File
                                (18 . (all-the-icons-material "collections_bookmark" :height 0.7 :face 'all-the-icons-red)) ;; Reference
                                (19 . (all-the-icons-material "folder" :height 0.7 :face 'all-the-icons-red)) ;; Folder
                                (20 . (all-the-icons-material "people" :height 0.7 :face 'all-the-icons-red)) ;; EnumMember
                                (21 . (all-the-icons-material "pause_circle_filled" :height 0.7 :face 'all-the-icons-red)) ;; Constant
                                (22 . (all-the-icons-material "streetview" :height 0.7 :face 'all-the-icons-red)) ;; Struct
                                (23 . (all-the-icons-material "event" :height 0.7 :face 'all-the-icons-red)) ;; Event
                                (24 . (all-the-icons-material "control_point" :height 0.7 :face 'all-the-icons-red)) ;; Operator
                                (25 . (all-the-icons-material "class" :height 0.7 :face 'all-the-icons-red)))
        company-box-icons-elisp
        (list
         (all-the-icons-material "functions" :height 0.7 :face 'all-the-icons-red)
         (all-the-icons-material "check_circle" :height 0.7 :face 'all-the-icons-blue)
         (all-the-icons-material "stars" :height 0.7 :face 'all-the-icons-orange)
         (all-the-icons-material "format_paint" :height 0.7 :face 'all-the-icons-pink))))
