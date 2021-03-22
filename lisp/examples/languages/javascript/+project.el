;;; lang-x/javascript/+project.el -*- lexical-binding: t; -*-

;;
;; Projects
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: Setting frontend projects
;; - Auto generate tags
;; - Auto check and install node modules
;; - Auto add `node_modules/.bin/' as exec path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-project-mode! +gatsby-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files ("gatsby-config.js")
  :add-hooks (+javascript|setup-gatsby-project))

(def-project-mode! +netlify-mode
  :modes (html-mode css-mode web-mode js2-mode markdown-mode)
  :files ("netlify.tmol")
  :add-hooks (+javascript|setup-netlify-project))

(def-project-mode! +web-meteor-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'meteor))

(def-project-mode! +web-vue-mode
  :modes (+javascript-npm-mode)
  :when (+javascript-npm-dep-p 'vue))
