;; -*- no-byte-compile: t; -*-
;;; private/chinese/packages.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chinese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (package! fcitx)
;; (package! cnfonts)
;; (package! chinese-wbim)
;; (package! find-by-pinyin-dired)
;; (package! ace-pinyin)
;; (package! pangu-spacing)
;; (package! pinyinlib)

(when (featurep! +input)
  (package! pyim)
  ; 拼音词库设置，五笔用户 *不需要* 此行设置
  (package! pyim-basedict))

(when (featurep! +calendar) (package! cal-china-x))
