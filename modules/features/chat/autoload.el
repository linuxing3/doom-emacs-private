;;; feature-x/chat/autoload.el -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;; TODO: Integrate company-emoji.

;;;###autoload
(defun spacemacs//slack-persp-filter-save-buffers-function (buffer)
  "Filter for slack layout."
  (with-current-buffer buffer
	(eq major-mode 'slack-mode)))

;;;###autoload
(defun spacemacs//slack-buffer-to-persp ()
  "Add buffer to slack layout."
  (persp-add-buffer (current-buffer)))

;;;###autoload
(defun slack/init-alert ()
  (use-package alert
	:defer t
	:init (setq alert-default-style 'notifier)))

;;;###autoload
(defun slack/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'slack-mode-hook 'emoji-cheat-sheet-plus-display-mode))


;;;###autoload
(defun slack/init-slack ()
  "Initialize Slack"
  (use-package slack
	:commands (slack-start)
	:defer t
	:init
	(progn
	  (map!
	   (:leader
		 (:desc "slack" :prefix "a"
		   :n "Cs" #'slack-start
		   :n "Cj" #'slack-channel-select
		   :n "Cg" #'slack-group-select
		   :n "Cr" #'slack-select-rooms
		   :n "Cd" #'slack-im-select
		   :n "Cq" #'slack-ws-close)))
	  (setq slack-enable-emoji t))
	:config
	(progn
	  (map! :map slack-mode-map
			"j" #'slack-channel-select
			"g" #'slack-group-select
			"r" #'slack-select-rooms
			"d" #'slack-im-select
			"p" #'slack-room-load-prev-messages
			"e" #'slack-message-edit
			"t" #'slack-thread-show-or-create
			"q" #'slack-ws-close
			"mm" #'slack-message-embed-mention
			"mc" #'slack-message-embed-channel
			"k" #'slack-select-rooms
			"@" #'slack-message-embed-mention
			"#" #'slack-message-embed-channel
			")" #'slack-message-add-reaction
			"(" #'slack-message-remove-reaction)
	  )))

;;; autoloads.el ends here
