;;; private/themes/config.el -*- lexical-binding: t; -*-

(if (featurep! +font) (load! "+font"))

(if (featurep! +modeline) (load! "+modeline"))

(if (featurep! +icon) (load! "+icon"))

(if (featurep! +theme) (load! "+theme"))

;; fancy title bar for Macos

(if IS-MAC
  (setq default-frame-alist
        '((tool-bar-lines . 0)
          (menu-bar-lines . 0)
          (ns-transparent-titlebar . t)
          (ns-appearance . dark)
          (background-color . "#21242b")
          (right-divider-width . 1)
          (bottom-divider-width . 1)
          (vertical-scroll-bars))))

(setq frame-title-format nil)
(setq ns-use-proxy-icon nil)
