;;; .emacs-profile.el -*- lexical-binding: t; -*-
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;
;;       ___           ___           ___           ___           ___           ___           ___
;;      /  /\         /__/\         /  /\         /__/\         /  /\         /  /\         /  /\
;;     /  /:/         \  \:\       /  /:/_       |  |::\       /  /::\       /  /:/        /  /:/_
;;    /  /:/           \__\:\     /  /:/ /\      |  |:|:\     /  /:/\:\     /  /:/        /  /:/ /\
;;   /  /:/  ___   ___ /  /::\   /  /:/ /:/_   __|__|:|\:\   /  /:/~/::\   /  /:/  ___   /  /:/ /::\
;;  /__/:/  /  /\ /__/\  /:/\:\ /__/:/ /:/ /\ /__/::::| \:\ /__/:/ /:/\:\ /__/:/  /  /\ /__/:/ /:/\:\
;;  \  \:\ /  /:/ \  \:\/:/__\/ \  \:\/:/ /:/ \  \:\~~\__\/ \  \:\/:/__\/ \  \:\ /  /:/ \  \:\/:/~/:/
;;   \  \:\  /:/   \  \::/       \  \::/ /:/   \  \:\        \  \::/       \  \:\  /:/   \  \0.1 /:/
;;    \  \:\/:/     \  \:\        \  \:\/:/     \  \:\        \  \:\        \  \:\/:/     \__\/ /:/
;;     \  \::/       \  \:\        \  \::/       \  \:\        \  \:\        \  \::/        /__/:/
;;      \__\/         \__\/         \__\/         \__\/         \__\/         \__\/         \__\/
;;
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;
;; Chemacs - Emacs Profile
;; https://github.com/plexus/chemacs2
;;
;; Usage:
;; git clone https://github.com/plexus/chemacs2.git ~/.emacs.d
;; echo 'doom' > ~/.emacs-profile
;;
;; Put this file in ~/.emacs.profiles.el
;;
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(("default" . ((user-emacs-directory . "c:/Users/Administrator/.emacs.d")
            (env . (("DOOMDIR" . "c:/Users/Administrator/.doom.d")))))
 ("spacemacs" . ((user-emacs-directory . "c:/Users/Administrator/.spacemacs")
                 (env . (("SPACEMACSDIR" . "~/.spacemacs.d")))))
 ("doom" . ((user-emacs-directory . "c:/Users/Administrator/.emacs.d")
           (env . (("DOOMDIR" . "c:/Users/Administrator/.doom.d")))))
 ("prelude" . ((user-emacs-directory . "c:/Users/Administrator/.prelude"))))
