;;; private/defaults/+bindings.el -*- lexical-binding: t; -*-

;; 基于SPACE的键设置

;; expand-region's prompt can't tell what key contract-region is bound to, so we
;; tell it explicitly.
(setq expand-region-contract-fast-key "V")

;;
(map! [remap evil-jump-to-tag] #'projectile-find-tag
      [remap find-tag]         #'projectile-find-tag
      [remap newline]          #'newline-and-indent

      ;; Ensure there are no conflicts
      :nmvo doom-leader-key nil
      :nmvo doom-localleader-key nil

      ;; --- Global keybindings ---------------------------
      ;; Make M-x available everywhere
      ;; M在Windows/linux系统下是Alt键，Mac系统下是Option键
      :gnvime "M-x" #'execute-extended-command
      :gnvime "A-x" #'execute-extended-command

      ;; A little sandbox to run code in
      :gnvime "M-;" #'eval-expression
      :gnvime "M-:" #'doom/open-scratch-buffer

      ;; Text-scaling
      "M-+"       (λ! (text-scale-set 0))
      "M-="       #'text-scale-increase
      "M--"       #'text-scale-decrease

      ;; Simple window/frame navigation/manipulation
      "C-`"       #'+popup/toggle ;;打开弹窗
      "C-~"       #'+popup/raise  ;;抬高弹窗
      "M-t"       #'+workspace/new ;;创建新工作区
      "M-T"       #'+workspace/display ;;显示工作区
      "M-w"       #'delete-window ;; 删除窗口
      "M-W"       #'delete-frame  ;; 删除桢
      "C-M-f"     #'toggle-frame-fullscreen ;;全屏切换
      "M-n"       #'evil-buffer-new ;;创建新缓存区
      "M-N"       #'make-frame ;;创建新的帧
      "M-1"       (λ! (+workspace/switch-to 0))
      "M-2"       (λ! (+workspace/switch-to 1))
      "M-3"       (λ! (+workspace/switch-to 2))
      "M-4"       (λ! (+workspace/switch-to 3))
      "M-5"       (λ! (+workspace/switch-to 4))
      "M-6"       (λ! (+workspace/switch-to 5))
      "M-7"       (λ! (+workspace/switch-to 6))
      "M-8"       (λ! (+workspace/switch-to 7))
      "M-9"       (λ! (+workspace/switch-to 8))
      "M-0"       #'+workspace/switch-to-last

      ;; Other functional keys
      "<f2>"  #'previous-buffer
      "<f4>"  #'+treemacs/toggle ;;explore
      "<f3>"  #'next-buffer
      "<f7>"  (lambda! (find-file "D:/Dropbox/config/bookmarks"));; 打开chrome书签
      "<f8>"  #'fill-paragraph ;;折行
      "<f9>"  #'org-capture ;;org抓取器
      "<f10>"  #'my-gridsome-create-newpost-empty;;建立新博客
      "<f12>"  #'my-blog-gridsome-deploy ;;发布博客

      ;; Other sensible, textmate-esque global bindings
      :ne "M-r"   #'+eval/buffer  ;; 运行代码
      :ne "C-M-r"   #'+eval/buffer  ;; 运行代码
      :ne "M-R"   #'+eval/region-and-replace ;; 运行代码片段
      :ne "M-b"   #'+eval/build   ;; 构建
      :ne "M-a"   #'mark-whole-buffer  ;; 全选
      :ne "M-c"   #'evil-yank     ;; 粘贴
      :ne "M-q"   (if (daemonp) #'delete-frame #'evil-quit-all)
      :ne "M-f"   #'swiper        ;; 查找
      :ne "M-z"   #'fill-paragraph ;; 折行
      :n  "M-s"   #'save-buffer   ;; 保存
      :n  "M-F"   #'+format/buffer;; 保存
      :m  "A-j"   #'+default:multi-next-line  ;; 多次下一行
      :m  "A-k"   #'+default:multi-previous-line     ;;多次前一行
      :nv "C-SPC" #'+evil:fold-toggle  ;;  切换折叠
      :gnvimer "M-v" #'clipboard-yank  ;;  粘贴
      ;;  快速窗口移动 Easier window navigation
      :en "C-h"   #'evil-window-left
      :en "C-j"   #'evil-window-down
      :en "C-k"   #'evil-window-up
      :en "C-l"   #'evil-window-right

      "C-x p"     #'+popup/other
      ;; 提高效率的快捷键
      ;; :i  "jk"    (lambda! (evil-force-normal-state) (save-buffer)) ;;退出编辑状态并保存

      ;; --- <leader> -------------------------------------
      (:leader
        :desc "Ex command"              :nv ";"  #'evil-ex ;;Vim类执行扩展命令
        :desc "M-x"                     :nv ":"  #'execute-extended-command ;;Emacs类执行扩展命令
        :desc "Extended command"        :n "SPC" #'execute-extended-command ;;
        :desc "Pop up scratch buffer"   :nv "X"  #'doom/open-scratch-buffer ;;打开涂鸦缓冲区
        :desc "Org Capture"             :nv "x"  #'org-capture ;;org抓取器
        ;;:desc "Reload Config"           :nv "r"  #'doom/reload ;; reload

        ;; Most commonly used
        :desc "Switch workspace buffer" :n ","   #'persp-switch-to-buffer ;;切换缓冲区
        :desc "Switch buffer"           :n "<"   #'switch-to-buffer ;;切换缓冲区
        :desc "Browse files"            :n "."   #'find-file ;;查找文件
        :desc "Toggle last popup"       :n "~"   #'+popup/toggle ;;切换最后弹窗
        :desc "Eval expression"         :n "`"   #'eval-expression ;;验证表达式
        :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor ;;
        :desc "Jump to bookmark"        :n "RET" #'bookmark-jump-other-window ;; 跳到书签
        :desc "Set bookmark"            :n "#"   #'bookmark-set  ;; 设置书签

        ;; C-u is used by evil
        :desc "Universal argument"      :n "u"  #'universal-argument
        :desc "window"                  :n "w"  evil-window-map

        ;; 所有向前的操作
        (:desc "previous..." :prefix "["
          :desc "Text size"             :nv "[" #'text-scale-decrease
          :desc "Buffer"                :nv "b" #'previous-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:previous-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-previous
          :desc "Error"                 :nv "e" #'previous-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-left
          :desc "Smart jump"            :nv "h" #'smart-backward
          :desc "Spelling error"        :nv "s" #'evil-prev-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-previous-word-generic)

        ;; 所有向后的操作
        (:desc "next..." :prefix "]"
          :desc "Text size"             :nv "]" #'text-scale-increase
          :desc "Buffer"                :nv "b" #'next-buffer
          :desc "Diff Hunk"             :nv "d" #'git-gutter:next-hunk
          :desc "Todo"                  :nv "t" #'hl-todo-next
          :desc "Error"                 :nv "e" #'next-error
          :desc "Workspace"             :nv "w" #'+workspace/switch-right
          :desc "Smart jump"            :nv "l" #'smart-forward
          :desc "Spelling error"        :nv "s" #'evil-next-flyspell-error
          :desc "Spelling correction"   :n  "S" #'flyspell-correct-word-generic)

        ;; 搜索
        (:desc "search" :prefix "/"
          :desc "Swiper"                :nv "/" #'swiper
          :desc "Imenu"                 :nv "i" #'imenu
          :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere
          :desc "Online providers"      :nv "o" #'+lookup/online-select)

        ;; 标签和工作区
        (:desc "workspace" :prefix "TAB"
          :desc "显示标签栏"                 :n "TAB" #'+workspace/display
          :desc "新建工作区"                 :n "n"   #'+workspace/new
          :desc "加载工作区"                 :n "l"   #'+workspace/load
          :desc "加载末回话"                 :n "L"   (λ! (+workspace/load-session))
          :desc "保存工作区"                 :n "s"   #'+workspace/save
          :desc "自动存回话"                 :n "S"   #'+workspace/save-session
          :desc "切换工作区"                 :n "."   #'+workspace/switch-to
          :desc "删全部缓冲"                 :n "x"   #'doom/kill-all-buffers
          :desc "删全部会话"                 :n "X"   #'+workspace/kill-session
          :desc "删除工作区"                 :n "d"   #'+workspace/delete
          :desc "加载新会话"                 :n "L"   #'+workspace/load-session
          :desc "下一工作区"                 :n "]"   #'+workspace/switch-right
          :desc "前一工作区"                 :n "["   #'+workspace/switch-left
          :desc "第一工作区"                 :n "1"   (λ! (+workspace/switch-to 0))
          :desc "第二工作区"                 :n "2"   (λ! (+workspace/switch-to 1))
          :desc "第三工作区"                 :n "3"   (λ! (+workspace/switch-to 2))
          :desc "第四工作区"                 :n "4"   (λ! (+workspace/switch-to 3))
          :desc "第五工作区"                 :n "5"   (λ! (+workspace/switch-to 4))
          :desc "第六工作区"                 :n "6"   (λ! (+workspace/switch-to 5))
          :desc "第七工作区"                 :n "7"   (λ! (+workspace/switch-to 6))
          :desc "第八工作区"                 :n "8"   (λ! (+workspace/switch-to 7))
          :desc "第九工作区"                 :n "9"   (λ! (+workspace/switch-to 8))
          :desc "最后工作区"                 :n "0"   #'+workspace/switch-to-last)

        ;; 缓冲区
        (:desc "buffer" :prefix "b"
          :desc "新建空缓冲"                 :n "n" #'evil-buffer-new
          :desc "切换区缓冲"                 :n "b" #'persp-switch-to-buffer
          :desc "切换缓冲区"                 :n "B" #'switch-to-buffer
          :desc "杀死缓冲区"                 :n "k" #'kill-this-buffer
          :desc "杀死它缓冲"                 :n "o" #'doom/kill-other-buffers
          :desc "保存缓冲区"                 :n "s" #'save-buffer
          :desc "弹出涂鸦板"                 :n "x" #'doom/open-scratch-buffer
          :desc "覆盖缓冲区"                 :n "z" #'bury-buffer
          :desc "下一缓冲区"                 :n "]" #'next-buffer
          :desc "前一缓冲区"                 :n "[" #'previous-buffer
          :desc "管理员编辑"                 :n "S" #'doom/sudo-this-file)

        ;; 代码 c
        (:desc "code" :prefix "c"
          :desc "创建项目标签"               :n  "t" (lambda! (my-setup-develop-environment "workspace" (read-from-minibuffer "FileName:")))
          :desc "显示错误信息"               :n  "x" #'flycheck-list-errors
          :desc "扩展宏的内容"               :n  "X" #'macrostep-expand
          :desc "执行缓冲内容"               :n  "e" #'+eval/buffer
          :desc "格式缓冲内容"               :n  "f" #'+format/buffer
          :v  "e" #'+eval/region
          :desc "执行替换区域"               :nv "E" #'+eval:replace-region
          :desc "执行构建任务"               :nv "b" #'+eval/build
          :desc "跳转查找定义"               :n  "d" #'+lookup/definition
          :desc "跳转查找引用"               :n  "D" #'+lookup/references
          :desc "打开交互环境"               :n  "r" #'+eval/open-repl
          :v  "r" #'+eval:repl)
        ;;  文件 f
        (:desc "file" :prefix "f"
          :desc "个人配置查找"                :n "1" #'+default/browse-config
          :desc "个人笔记查找"                :n "2" #'+default/browse-notes
          :desc "个人代码查找"                :n "3" #'+default/browse-snippets
          :desc "个人模板查找"                :n "4" #'+default/browse-templates
          :desc "找到打开文件"                :n "." #'find-file
          :desc "超级打开文件"                :n ">" #'doom/sudo-find-file
          :desc "搜索项目文件"                :n "/" #'projectile-find-file
          :desc "本地搜索文件"                :n "?" #'counsel-file-jump
          :desc "查找别的文件"                :n "a" #'projectile-find-other-file
          :desc "查编辑器配置"                :n "c" #'editorconfig-find-current-editorconfig
          :desc "文件目录浏览"                :n "d" #'dired
          :desc "配置目录查找"                :n "e" #'+default/find-in-emacsd
          :desc "配置目录浏览"                :n "E" #'+default/browse-emacsd
          :desc "最近使用文件"                :n "r" #'recentf-open-files
          :desc "最近使用项目"                :n "R" #'projectile-recentf
          :desc "插入文件名称"                :n "y" #'+default/yank-buffer-filename
          )

        ;; git g
        (:desc "git" :prefix "g"
          :desc "Git status"            :n  "S" #'magit-status
          :desc "Git blame"             :n  "b" #'magit-blame
          :desc "Git time machine"      :n  "t" #'git-timemachine-toggle
          :desc "Git stage hunk"        :n  "s" #'git-gutter:stage-hunk
          :desc "Git revert hunk"       :n  "r" #'git-gutter:revert-hunk
          :desc "Git revert buffer"     :n  "R" #'vc-revert
          :desc "List gists"            :n  "g" #'+gist:list
          :desc "Next hunk"             :nv "]" #'git-gutter:next-hunk
          :desc "Previous hunk"         :nv "[" #'git-gutter:previous-hunk)

        ;;  帮助 h
        (:desc "help" :prefix "h"
          :n "h" help-map
          :desc "Apropos"               :n  "a" #'apropos
          :desc "Reload theme"          :n  "R" #'doom//reload-theme
          :desc "Install packages"      :n  "I" #'doom/install-package
          :desc "Refesh packages"       :n  "I" #'doom/refresh-packages
          :desc "Find library"          :n  "l" #'find-library
          :desc "Toggle Emacs log"      :n  "m" #'view-echo-area-messages
          :desc "Command log"           :n  "L" #'global-command-log-mode
          :desc "Describe function"     :n  "f" #'describe-function
          :desc "Describe key"          :n  "k" #'describe-key
          :desc "Describe char"         :n  "c" #'describe-char
          :desc "Describe mode"         :n  "M" #'describe-mode
          :desc "Describe variable"     :n  "v" #'describe-variable
          :desc "Describe face"         :n  "F" #'describe-face
          :desc "Describe DOOM setting" :n  "s" #'doom/describe-setting
          :desc "Describe DOOM module"  :n  "d" #'doom/describe-module
          :desc "Open Doom manual"      :n  "D" #'doom/help
          :desc "Find definition"       :n  "." #'+lookup/definition
          :desc "Find references"       :n  "/" #'+lookup/references
          :desc "Find documentation"    :n  "h" #'+lookup/documentation
          :desc "Describe at point"     :n  "." #'helpful-at-point
          :desc "What face"             :n  "'" #'doom/what-face
          :desc "What minor modes"      :n  ";" #'doom/what-minor-mode
          :desc "Info"                  :n  "i" #'info
          :desc "Toggle profiler"       :n  "p" #'doom/toggle-profiler)

        ;; 插入 i
        (:desc "insert" :prefix "i"
          :desc "From kill-ring"        :nv "y" #'yank-pop
          :desc "From snippet"          :nv "s" #'yas-insert-snippet)

        ;; 笔记 n
        (:desc "notes" :prefix "n"
          :desc "Find file in notes"    :n  "n" #'+default/find-in-notes
          :desc "Browse notes"          :n  "N" #'+default/browse-notes
          :desc "Org capture"           :n  "x" #'+org-capture/open
          :desc "Browse mode notes"     :n  "m" #'+org/browse-notes-for-major-mode
          :desc "Browse project notes"  :n  "p" #'+org/browse-notes-for-project)

        ;; 打开 a
        (:desc "open" :prefix "a"
          :desc "Default browser"       :n  "b" #'browse-url-of-file
          :desc "Debugger"              :n  "d" #'+debug/open
          :desc "REPL"                  :n  "r" #'+eval/open-repl
          :v  "r" #'+eval:repl
          :desc "Neotree"               :n  "n" #'+neotree/open
          :desc "Neotree: on this file" :n  "N" #'+neotree/find-this-file
          :desc "Imenu sidebar"         :nv "i" #'imenu-list-minor-mode
          :desc "Terminal"              :n  "t" #'+term/open-popup
          :desc "Terminal in project"   :n  "T" #'+term/open-popup-in-project

          ;; applications
          :desc "APP: elfeed"           :n "E" #'=rss
          :desc "APP: email"            :n "M" #'=email
          :desc "APP: twitter"          :n "T" #'=twitter
          :desc "APP: regex"            :n "X" #'=regex

          ;; macos
          (:when IS-MAC
            :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
            :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
            :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
            :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
            :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
            :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

        ;; 工程 p
        (:desc "project" :prefix "p"
          :desc "Browse project"          :n  "." #'+default/browse-project
          :desc "Find file in project"    :n  "/" #'projectile-find-file
          :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
          :desc "Compile project"         :n  "c" #'projectile-compile-project
          :desc "Find other file"         :n  "o" #'projectile-find-other-file
          :desc "Switch project"          :n  "p" #'projectile-switch-project
          :desc "Recent project files"    :n  "r" #'projectile-recentf
          :desc "List project tasks"      :n  "t" #'+ivy/tasks
          :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

        (:desc "quit" :prefix "q"
          :desc "Save and quit"          :n "q" #'evil-save-and-quit
          :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

        (:when (featurep! :tools upload)
          (:desc "remote" :prefix "r"
            :desc "Upload local"           :n "u" #'ssh-deploy-upload-handler
            :desc "Upload local (force)"   :n "U" #'ssh-deploy-upload-handler-forced
            :desc "Download remote"        :n "d" #'ssh-deploy-download-handler
            :desc "Diff local & remote"    :n "D" #'ssh-deploy-diff-handler
            :desc "Browse remote files"    :n "." #'ssh-deploy-browse-remote-handler
            :desc "Detect remote changes"  :n ">" #'ssh-deploy-remote-changes-handler))

        ;; 代码片段 s
        (:desc "snippets" :prefix "s"
          :desc "New snippet"            :n  "n" #'yas-new-snippet
          :desc "Insert snippet"         :nv "i" #'yas-insert-snippet
          :desc "Find snippet for mode"  :n  "s" #'yas-visit-snippet-file
          :desc "Find snippet"           :n  "S" #'+default/find-in-snippets)

        ;; 切换 t
        (:desc "toggle" :prefix "t"
          :desc "Flyspell"               :n "s" #'flyspell-mode
          :desc "Flycheck"               :n "f" #'flycheck-mode
          :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
          :desc "Frame fullscreen"       :n "F" #'toggle-frame-fullscreen
          :desc "Indent guides"          :n "i" #'highlight-indentation-mode
          :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
          :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
          :desc "Big mode"               :n "b" #'doom-big-font-mode
          :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))


      ;; --- Personal vim-esque bindings ------------------
      :n  "zx" #'kill-this-buffer ;杀死当前缓冲
      :n  "ZX" #'bury-buffer      ;去掉当前缓冲
      :n  "]b" #'next-buffer      ;下个缓冲
      :n  "[b" #'previous-buffer  ;枪个缓冲
      :n  "]w" #'+workspace/switch-right ;切换右边工作区
      :n  "[w" #'+workspace/switch-left  ;切换左边工作区
      :m  "gt" #'+workspace/switch-right ;
      :m  "gT" #'+workspace/switch-left  ;
      :m  "gd" #'+lookup/definition      ;查找定义
      :m  "gD" #'+lookup/references      ;查找引用
      :m  "gh" #'+lookup/documentation   ;查看文裆
      :n  "gp" #'+evil/reselect-paste    ;重新选择-粘贴
      :n  "gr" #'+eval:region            ;执区域
      :n  "gR" #'+eval/buffer            ;执缓冲
      :v  "gR" #'+eval:replace-region    ;替换区域
      :v  "@"  #'+evil:macro-on-all-lines;执行宏
      :n  "g@" #'+evil:macro-on-all-lines
      ;; repeat in visual mode (FIXME buggy)
      :v  "."  #'evil-repeat             ;点重复执行
      ;; don't leave visual mode after shifting
      :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
      :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
      ;; paste from recent yank register (which isn't overwritten)
      :v  "C-p" "\"0p"                ;从最新寄存器粘贴

      :nv "C-a" #'evil-numbers/inc-at-pt ;增加编号
      :nv "C-x" #'evil-numbers/dec-at-pt ;缩小编号


      ;; --- Plugin bindings ------------------------------
      ;; auto-yasnippet
      :i  [C-tab] #'aya-expand  ;; 自动片段扩展
      :nv [C-tab] #'aya-create  ;; 创建代码片段

      ;;  补全功能 company-mode (vim-like omnicompletion)
      :i [tab]    #'+company/complete  ;; 文字表达式补全
      :i "M-/"    #'+company/complete  ;; 文字表达式补全
      (:prefix "C-x"
        :i "C-l"   #'+company/whole-lines ;; 行补全
        :i "C-k"   #'+company/dict-or-keywords  ;; 字典补全
        :i "C-f"   #'company-files  ;; 文件补全
        :i "C-]"   #'company-etags  ;; 标签补全
        :i "s"     #'company-ispell ;; 拼写补全
        :i "C-s"   #'company-yasnippet  ;; 代码片段补全
        :i "C-o"   #'company-capf
        :i "C-n"   #'company-dabbrev-code  ;; 简写缩写补全
        :i "C-p"   #'+company/dabbrev-code-previous)
      (:after company
        (:map company-active-map
          ;; Don't interfere with `evil-delete-backward-word' in insert mode
          "C-w"        nil
          "C-o"        #'company-search-kill-others
          "C-n"        #'company-select-next
          "C-p"        #'company-select-previous
          "C-h"        #'company-quickhelp-manual-begin
          "C-S-h"      #'company-show-doc-buffer
          "C-S-s"      #'company-search-candidates
          "C-s"        #'company-filter-candidates
          "C-SPC"      #'company-complete-common ; 常用补全
          "C-h"        #'company-quickhelp-manual-begin ; 快捷帮助
          [tab]        #'company-complete-common-or-cycle ;常用补全
          [backtab]    #'company-select-previous
          [escape]     (λ! (company-abort) (evil-normal-state 1)))
        ;; Automatically applies to `company-filter-map'
        (:map company-search-map
          "C-n"        #'company-search-repeat-forward
          "C-p"        #'company-search-repeat-backward
          "C-s"        (λ! (company-search-abort) (company-filter-candidates))
          [escape]     #'company-search-abort))

      ;; counsel
      (:after counsel
        (:map counsel-ag-map
          [backtab]  #'+ivy/wgrep-occur      ; 查找替换结果
          "C-SPC"    #'ivy-call-and-recenter ; 预览
          ))

      ;; evil
      (:after evil
        :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
        :textobj "B" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
        :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
        :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
        :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down

        (:map evil-window-map ; prefix "C-w"
          ;; Navigation
          "C-h"     #'evil-window-left ;选择左窗口
          "C-j"     #'evil-window-down ;选择右窗口
          "C-k"     #'evil-window-up   ;选择上窗口
          "C-l"     #'evil-window-right;选择右窗口
          "C-w"     #'other-window
          ;; Swapping windows
          "H"       #'+evil/window-move-left ;移动窗口到左边
          "J"       #'+evil/window-move-down ;移动窗口到下边
          "K"       #'+evil/window-move-up   ;移动窗口到上边
          "L"       #'+evil/window-move-right;移动窗口到右边
          "C-S-w"   #'ace-swap-window
          ;; Window undo/redo
          "u"       #'winner-undo
          "C-u"     #'winner-undo
          "C-r"     #'winner-redo
          "o"       #'doom/window-enlargen
          ;; Delete window
          "c"       #'+workspace/close-window-or-workspace
          "C-C"     #'ace-delete-window))

      ;; evil-commentary 注释
      :n  "gc"  #'evil-commentary    ;切换注释
      ;; :i  "C-/" #'evil-commentary    ;切换注释

      ;; evil-exchange
      :n  "gx"  #'evil-exchange      ;交换

      ;; evil-matchit
      :nv "gf" #'+evil/matchit-or-toggle-fold

      ;; evil-magit
      (:after evil-magit
        :map (magit-status-mode-map magit-revision-mode-map)
        :n "C-j" nil
        :n "C-k" nil)

      ;; evil-mc 多光标移动
      (:prefix "gz"
        :nv "m" #'evil-mc-make-all-cursors ;标记全部
        :nv "u" #'evil-mc-undo-all-cursors ;取消全部
        :nv "z" #'+evil/mc-make-cursor-here ;标记当前位置
        :nv "t" #'+evil/mc-toggle-cursors   ;切换光标
        :nv "n" #'evil-mc-make-and-goto-next-cursor
        :nv "p" #'evil-mc-make-and-goto-prev-cursor
        :nv "N" #'evil-mc-make-and-goto-last-cursor
        :nv "P" #'evil-mc-make-and-goto-first-cursor
        :nv "d" #'evil-mc-make-and-goto-next-match
        :nv "D" #'evil-mc-make-and-goto-prev-match)

      (:after evil-mc
        :map evil-mc-key-map
        :nv "C-n" #'evil-mc-make-and-goto-next-cursor
        :nv "C-N" #'evil-mc-make-and-goto-last-cursor
        :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
        :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

      ;; 多重编辑 evil-multiedit
      :v  "R"     #'evil-multiedit-match-all ;;选择全部批配内容
      :n  "M-d"   #'evil-multiedit-match-symbol-and-next ;连续选择当前单词
      :n  "M-D"   #'evil-multiedit-match-symbol-and-prev ;反向连续选择
      :v  "M-d"   #'evil-multiedit-match-and-next        ;
      :v  "M-D"   #'evil-multiedit-match-and-prev        ;
      :nv "C-M-d" #'evil-multiedit-restore
      (:after evil-multiedit
        (:map evil-multiedit-state-map
          "M-d" #'evil-multiedit-match-and-next
          "M-D" #'evil-multiedit-match-and-prev
          "RET" #'evil-multiedit-toggle-or-restrict-region)
        (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
          "C-n" #'evil-multiedit-next
          "C-p" #'evil-multiedit-prev))

      ;; evil-snipe
      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        ;; switch to evil-easymotion/avy after a snipe
        "C-;" (λ! (require 'evil-easymotion)
                  (call-interactively
                   (evilem-create #'evil-snipe-repeat
                                  :bind ((evil-snipe-scope 'whole-buffer)
                                         (evil-snipe-enable-highlight)
                                         (evil-snipe-enable-incremental-highlight))))))

      ;; evil-surround
      :v  "S"  #'evil-surround-region
      :o  "s"  #'evil-surround-edit
      :o  "S"  #'evil-Surround-edit

      ;; expand-region
      :v  "v"  #'er/expand-region
      :v  "V"  #'er/contract-region

      ;; flycheck
      :m  "]e" #'next-error
      :m  "[e" #'previous-error
      (:after flycheck
        :map flycheck-error-list-mode-map
        :n "C-n" #'flycheck-error-list-next-error
        :n "C-p" #'flycheck-error-list-previous-error
        :n "j"   #'flycheck-error-list-next-error
        :n "k"   #'flycheck-error-list-previous-error
        :n "RET" #'flycheck-error-list-goto-error)

      ;; flyspell
      :m  "]S" #'flyspell-correct-word-generic
      :m  "[S" #'flyspell-correct-previous-word-generic
      (:after flyspell
        ;; Press RET on misspelled words to correct them
        (:map flyspell-mouse-map
          "RET" #'flyspell-correct-word-generic
          "<mouse-1>" #'flyspell-correct-word-generic))

      ;; git-gutter
      :m  "]d" #'git-gutter:next-hunk
      :m  "[d" #'git-gutter:previous-hunk

      ;; git-timemachine
      (:after git-timemachine
        (:map git-timemachine-mode-map
          :n "C-p" #'git-timemachine-show-previous-revision
          :n "C-n" #'git-timemachine-show-next-revision
          :n "[["  #'git-timemachine-show-previous-revision
          :n "]]"  #'git-timemachine-show-next-revision
          :n "q"   #'git-timemachine-quit
          :n "gb"  #'git-timemachine-blame))

      ;; gist
      (:after gist
        :map gist-list-menu-mode-map
        :n "RET" #'+gist/open-current
        :n "b"   #'gist-browse-current-url
        :n "c"   #'gist-add-buffer
        :n "d"   #'gist-kill-current
        :n "f"   #'gist-fork
        :n "q"   #'quit-window
        :n "r"   #'gist-list-reload
        :n "s"   #'gist-star
        :n "S"   #'gist-unstar
        :n "y"   #'gist-print-current-url)

      ;; helm
      (:after helm
        (:map helm-map
          "ESC"        nil
          "C-S-n"      #'helm-next-source
          "C-S-p"      #'helm-previous-source
          "C-u"        #'helm-delete-minibuffer-contents
          "C-w"        #'backward-kill-word
          "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
          "C-b"        #'backward-word
          [left]       #'backward-char
          [right]      #'forward-char
          [escape]     #'helm-keyboard-quit
          [tab]        #'helm-execute-persistent-action)

        (:after helm-files
          (:map helm-generic-files-map
            :e "ESC"     #'helm-keyboard-quit)
          (:map helm-find-files-map
            "C-w" #'helm-find-files-up-one-level
            "TAB" #'helm-execute-persistent-action))

        (:after helm-ag
          (:map helm-ag-map
            "<backtab>"  #'helm-ag-edit)))

      ;; hl-todo
      :m  "]t" #'hl-todo-next
      :m  "[t" #'hl-todo-previous

      ;; ivy
      (:after ivy
        :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-SPC" #'ivy-call-and-recenter
        "M-v" #'yank
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "C-l" #'ivy-alt-done
        "C-w" #'ivy-backward-kill-word
        "C-u" #'ivy-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word)

      ;; neotree
      (:after neotree
        :map neotree-mode-map
        :n "g"         nil
        :n [tab]       #'neotree-quick-look
        :n "RET"       #'neotree-enter
        :n [backspace] #'evil-window-prev
        :n "c"         #'neotree-create-node ;; 创建
        :n "r"         #'neotree-rename-node ;; 重命名
        :n "d"         #'neotree-delete-node ;; 删除
        :n "j"         #'neotree-next-line   ;; 下一项
        :n "k"         #'neotree-previous-line ;; 上一项
        :n "n"         #'neotree-next-line  ;; 下一项
        :n "p"         #'neotree-previous-line ;; 上一项
        :n "h"         #'+neotree/collapse-or-up ;; 收起
        :n "l"         #'+neotree/expand-or-open ;; 展开
        :n "J"         #'neotree-select-next-sibling-node ;; 下一大项
        :n "K"         #'neotree-select-previous-sibling-node ;; 上一大项
        :n "H"         #'neotree-select-up-node ;; 上一级目录
        :n "L"         #'neotree-select-down-node ;; 下一级目录
        :n "G"         #'evil-goto-line
        :n "gg"        #'evil-goto-first-line
        :n "v"         #'neotree-enter-vertical-split ;; 纵向分割
        :n "s"         #'neotree-enter-horizontal-split ;; 横向风格
        :n "q"         #'neotree-hide
        :n "R"         #'neotree-refresh)

      ;; realgud
      (:after realgud
        :map realgud:shortkey-mode-map
        :n "j" #'evil-next-line
        :n "k" #'evil-previous-line
        :n "h" #'evil-backward-char
        :n "l" #'evil-forward-char
        :m "n" #'realgud:cmd-next
        :m "b" #'realgud:cmd-break
        :m "B" #'realgud:cmd-clear
        :n "c" #'realgud:cmd-continue)

      ;; rotate-text
      :n  "!"  #'rotate-text

      ;; smart-forward
      :nv "K"  #'smart-up
      :m  "g]" #'smart-forward
      :m  "g[" #'smart-backward

      ;; swiper
      (:after swiper
        (:map swiper-map
          [backtab]  #'+ivy/wgrep-occur))

      ;; undo-tree -- undo/redo for visual regions
      :v "C-u" #'undo-tree-undo ; NOTE: 取消
      :v "C-r" #'undo-tree-redo ; NOTE: 重复

      ;; 代码片段 yasnippet
      (:after yasnippet
        (:map yas-keymap
          "C-e"           #'+snippets/goto-end-of-field
          "C-a"           #'+snippets/goto-start-of-field
          "<M-right>"     #'+snippets/goto-end-of-field
          "<M-left>"      #'+snippets/goto-start-of-field
          "<M-backspace>" #'+snippets/delete-to-start-of-field
          [backspace]     #'+snippets/delete-backward-char
          [delete]        #'+snippets/delete-forward-char-or-field)
        (:map yas-minor-mode-map
          :ig "<tab>" yas-maybe-expand   ;; 使用 Tab 进行自动补全
          :v  "<tab>" #'+snippets/expand-on-region))


      ;; --- Major mode bindings --------------------------
      (:after markdown-mode
        (:map markdown-mode-map
          ;; fix conflicts with private bindings
          "<backspace>" nil
          "<M-left>"    nil
          "<M-right>"   nil))


      ;; --- Built-in plugins -----------------------------
      (:after comint
        ;; TAB auto-completion in term buffers
        :map comint-mode-map [tab] #'company-complete)

      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)

      (:map help-mode-map
        :n "[["  #'help-go-back
        :n "]]"  #'help-go-forward
        :n "o"   #'ace-link-help
        :n "q"   #'quit-window
        :n "Q"   #'ivy-resume)

      (:after vc-annotate
        :map vc-annotate-mode-map
        :n "q"   #'kill-this-buffer
        :n "d"   #'vc-annotate-show-diff-revision-at-line
        :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
        :n "SPC" #'vc-annotate-show-log-revision-at-line
        :n "]]"  #'vc-annotate-next-revision
        :n "[["  #'vc-annotate-prev-revision
        :n "TAB" #'vc-annotate-toggle-annotation-visibility
        :n "RET" #'vc-annotate-find-revision-at-line))


;;
;; 键绑定修补 Keybinding fixes
;;


(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      ;; C-a / C-e 更加智能
      ;; C-a 一次跳转到缩进,按两次到真正行首
      ;; C-e 一次到缩进,两次到行尾,忽略注释和行尾空格
      :i "C-a" #'doom/backward-to-bol-or-indent ;两按到行首,一按缩进首
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol ;两按到行尾,一按无注释

      ;; textmate-esque 删除
      :ig [M-backspace] #'doom/backward-kill-to-bol-and-indent
      ;; doom enhaced 删除
      :i "C-u" #'doom/backward-kill-to-bol-and-indent ;删除到行首或缩进位
      :i "C-k" #'doom/forward-kill-to-eol ;删除到行首或缩进位

      :i "C-n" #'evil-next-line
      :i "C-p" #'evil-previous-line

      :in "C-s" #'save-buffer

      ;; Emacsien motions for insert mode
      :i  "C-b" #'backward-word
      :i  "C-f" #'forward-word

      ;; textmate-esque 插入新行
      :i  [M-return]    #'evil-open-below
      :i  [S-M-return]  #'evil-open-above

      ;; minibuffer中恢复健盘位
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:after evil
        (:map evil-ex-completion-map
          "C-a" #'move-beginning-of-line))

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:after tabulated-list
        (:map tabulated-list-mode-map
          [remap evil-record-macro] #'quit-window))

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))
