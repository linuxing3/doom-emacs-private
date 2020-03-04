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
;;
;; ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

(
 ;; Doom as Default to WINDOWS HOME
 ("default" . ((env . (("DOOMDIR" . "~/.doom.d")))
               (user-emacs-directory . "~/.emacs.d")
               (doom-private-dir . "~/.doom.d")))

 ;; Spacemacs to other dir
 ("spacemacs" . ((env . (("HOME" . "D:/Users/root")
                         ("EMACSDIR" . "D:/Users/root/.emacs.d")
                         ("EMACS_SERVER_FILE" . "D:/Users/root/.emacs.d/.cache/server/server")
                         ("SPACEMACSDIR" . "D:/Users/root/.spacemacs.d")))
                 (server-name . "D:/Users/root/.emacs.d/.cache/server/server")
                 (user-emacs-directory . "D:/Users/root/.emacs.d")))

 ;; Doom to C drive with default home dir
 ("doom" . ((env . (("HOME" . "C:/Users/Wjb")
                    ("EMACSDIR" . "C:/Users/Wjb/.emacs.d")
                    ("EMACS_SERVER_FILE" . "c:/Users/Wjb/.emacs.d/.local/cache/server/server")
                    ("DOOMDIR" . "C:/Users/Wjb/.doom.d")))
            (user-emacs-directory . "C:/Users/Wjb/.emacs.d")
            (server-name . "c:/Users/Wjb/.emacs.d/.local/cache/server/server")
            (doom-private-dir . "C:/Users/wjb/.doom.d")))

 ;; Simplified Doom to D drive with customized home dir
 ("cleandoom" . (
                 (env . (("HOME" . "D:/Users/xingwenju")
                         ("EMACSDIR" . "D:/Users/xingwenju/.emacs.d")
                    ("EMACS_SERVER_FILE" . "D:/Users/xingwenju/.emacs.d/.local/cache/server/server")
                         ("DOOMDIR" . "D:/Users/xingwenju/.doom.d")))
                 (user-emacs-directory . "D:/Users/xingwenju/.emacs.d")
                 (server-name . "D:/Users/xingwenju/.emacs.d/.local/cache/server/server")
                 (doom-private-dir . "D:/Users/xingwenju/.doom.d")))
 )
