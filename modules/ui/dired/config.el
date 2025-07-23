;;; ui/dired/config.el -*- lexical-binding: t; -*-

(set-fontset-font t 'unicode "Noto Color Emoji" nil 'append)

(use-package dirvish
  :after dired
  :init
  (dirvish-override-dired-mode)
  :custom
  (map! :map dired-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "a" #'dired-create-empty-file
        :n "A" #'dired-rename-file
        :n "TAB" #'dirvish-toggle-subtree
        :n "s" #'dirvish-quicksort
        :n "/" #'dirvish-filter
        :n "i" #'dired-subtree-insert
        :n "k" #'dired-subtree-remove)
  (setq dirvish-bookmark-entries
        '(("Downloads" "~/Downloads/")
          ("Dotfiles" "~/.config/")
          ("Persist" "/persistent/home/linuxing3/")
          ("Projects" "~/sources/")))

  (dirvish-header-line-format '(:left (path)))
  (dirvish-mode-line-format '(:left (info)))
  (dirvish-preview-dispatchers (list 'image 'pdf))
  :config
  (setq dirvish-mode-line-format
        '(:left (index) :right (details)))
  (setq inhibit-compacting-font-caches t)
  (setq dirvish-attributes '(all-the-icons file-time file-size git))
  (setq dirvish-hide-details t)
  (setq delete-by-moving-to-trash t)
  ;; optional: replace traditional dired commands
  (dirvish-override-dired-mode))

(use-package dired
  :defer t
  :hook (dired-mode . dired-hide-details-mode)
  :config
  (setq global-auto-revert-non-file-buffers t)
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  (add-hook 'dired-mode-hook #'dired-omit-mode)

  (setq dired-listing-switches "-alh --group-directories-first"))

(use-package direnv
  :hook (prog-mode . direnv--auto-allow)
  :config
  (direnv-mode)
  (add-hook 'projectile-after-switch-project-hook #'direnv--auto-allow)
  (defun direnv--auto-allow ()
    "Automatically run `direnv allow` if `.envrc` exists."
    (when (and buffer-file-name
               (locate-dominating-file buffer-file-name ".envrc"))
      (start-process "direnv-allow" "*direnv-allow*"
                     "/etc/profiles/per-user/linuxing3/bin/direnv" "allow"))))

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  :config
  (setq diredfl-light-blue "#5fafd7"))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (setq dired-hide-dotfiles-verbose nil))

(use-package dired-subtree
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              ("k" . dired-subtree-remove)))

(use-package dired-ranger
  :after dired)

(use-package dired-quick-sort
  :hook (dired-mode . dired-quick-sort-setup))

(use-package async
  :config
  (dired-async-mode 1))

(after! dired
  (require 'dired-x)
  ;; Optional: set default target directory for copy/move
  (setq dired-dwim-target t)
  ;; Enable omit mode by default to hide dotfiles
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-files "^\\.[^.].*"))  ;; hide dotfiles except '..'
