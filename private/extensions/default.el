;;; private/extensions/default.el -*- lexical-binding: t; -*-

(use-package! hydra)

(load! "exwm" nil t)

(load! "ui+font")

(load! "keybinding+helix" nil t)

(load! "mail" nil t)

(load! "feed" nil t)

(after! org
  (load! "org+config" nil t)
  (load! "org+capture" nil t)
  (load! "org+agenda" nil t)
  (load! "org+babel" nil t)
  (load! "org+publish" nil t))

(load! "consult")
