;;; private/xingwenju/config.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (featurep! +news) (load! "+news"))
(if (featurep! +rss) (load! "+rss"))
(if (featurep! +research) (load! "+research"))
(if (featurep! +translate) (load! "+translate"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Blog
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (featurep! +blog)
    (progn
      ;; Blog with Hugo
      (setq blog-dir (os-path "~/Dropbox/xingwenju.com/hugo"))
      (setq blog-base-dir (os-path "~/Dropbox/xingwenju.com/hugo/posts"))
      (setq blog-public-dir (os-path "~/Dropbox/xingwenju.com/hugo/public"))
      ;; Blog with Gridsome
      (setq blog-gridsome-base-dir (os-path "~/workspace/gridsome.org"))
      ;; Blog with Gatsby
      (setq blog-gatsby-base-dir (os-path "~/workspace/gatsby-starter-netlify-cms"))
      ;; Blog with Vuepress
      (setq blog-vuepress-dir (os-path "~/workspace/cp-work-vue-starter"))
      (setq blog-vuepress-base-dir (os-path "~/workspace/awesome-manager/docs/zh/guide"))
      (setq blog-vuepress-public-dir (os-path "~/workspace/awesome-manager/docs/.vuepress/dist"))
      ;; Submodules
      (load! "+blog")))

;; Medium
(when (featurep! +medium)
  ;;(babel-load-if-exists "org-sendto-medium.org")
  (load! "org-sendto-medium"))

(def-package! org-sendto-medium
  :commands org-sendto-medium
  :config
  (define-key org-mode-map (kbd "C-x C-j") 'org-sendto-medium))
