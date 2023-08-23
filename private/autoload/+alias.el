;; make frequently used commands short
(defalias 'a1 'gtd "Alias: Open my GTD Inbox")
(defalias 'a2 'tutorial "Alias: Open my GTD Tutorial")
(defalias 'a3 'work "Alias: Open my Work file")
(defalias 'a4 'lulu "Alias: Open lulu file")
(defalias 'a5 'daniel "Alias: Open Daniel file")
(defalias 'a5 'me "Alias: Open my private file")

(defalias 'cc 'calc "Alias: Open calculator")
(defalias 'dml 'delete-matching-lines "Alias: Delete matching lines")
(defalias 'dnml 'delete-non-matching-lines "Alias: Delete not matching lines")
(defalias 'dr 'doom/reload "Alias: Reload doom")
(defalias 'dtw 'delete-trailing-whitespace "Alias: Delete trailing whitespace")
(defalias 'fb 'flyspell-buffer "Alias: Spell check buffer")
(defalias 'fd 'find-dired "Alias: Find in directory")
(defalias 'fp 'fill-paragraph "Alias: Fill paragraph")
(defalias 'fr 'fill-region "Alias: Fill selected region")
(defalias 'g 'grep "Alias: Grep and find")
(defalias 'gf 'grep-find "Alias: Grep and find")
(defalias 'lcd 'list-colors-display "Alias: List colors Desplay")
(defalias 'lml 'list-matching-lines "Alias: List matching lines")
(defalias 'qrr 'query-replace-regexp "Alias: query and replace with regular expression")
(defalias 'rb 'revert-buffer "Alias: Reload buffer from revert file")
(defalias 'rof 'recentf-open-files "Alias: Open recent files")
(defalias 'rr 'reverse-region "Alias: Reverse region")
(defalias 'rs 'replace-string "Alias: Replace string")
(defalias 'sbc 'set-background-color "Alias: Set background Color")
(defalias 'sh 'shell "Alias: Open shell")
(defalias 'sl 'sort-lines "Alias: Sort lines")
(defalias 'www 'browse-url-default-browser "Alias: Browe with chrome")

; elisp
(defalias 'eb 'eval-buffer "Alias: Eval buffer")
(defalias 'ed 'eval-defun "Alias: Eval functions")
(defalias 'eis 'elisp-index-search "Alias: Elisp index search")
(defalias 'er 'eval-region "Alias: Eval region")
(defalias 'lf 'load-file "Alias: Load a elisp file")

; major modes
(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'tm 'text-mode)

; minor modes
(defalias 'glm 'global-linum-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'wsm 'whitespace-mode)

(define-abbrev-table 'global-abbrev-table '(
                                            ("8in" "∈")
                                            ("8nin" "∉")
                                            ("8inf" "∞")
                                            ("8luv" "♥")
                                            ("8smly" "☺")
                                            ("8en" "@~english")
                                            ("8zh" "@~chinese")
                                            ("8sp" "spacemacs")
                                            ;; email
                                            ("8me" "xingwenju@gmail.com")

                                            ;; computing tech
                                            ("8wp" "Wikipedia")
                                            ("8ms" "Microsoft")
                                            ("8g" "Google")
                                            ("8it" "IntelliType")
                                            ("8msw" "Microsoft Windows")
                                            ("8win" "Windows")
                                            ("8ie" "Internet Explorer")
                                            ("8ahk" "AutoHotkey")

                                            ;; signature
                                            ("8xing" "linuxing3<linuxing3@qq.com>")
                                            ;; emacs regex
                                            ("8d" "\\([0-9]+?\\)")
                                            ("8str" "\\([^\"]+?\\)\"")))
