;;; private/writer/+gmail.el -*- lexical-binding: t; -*-

(require 'smtpmail)
(setq
 user-mail-address "xignwenju@gmail.com"
 user-full-name  "Xing Wenju")

(if IS-LINUX
    (def-package! mu4e
                  :config
                  ;; mu4e-drafts-folder
                  (setq mu4e-maildir "~/Maildir")
                  (setq mu4e-drafts-folder "/Drafts")
                  (setq mu4e-sent-folder   "/Sent Mail")
                  (setq mu4e-trash-folder  "/Trash")
                  (setq mu4e-sent-messages-behavior 'delete)
                  (setq mu4e-maildir-shortcuts
                        '( ("/INBOX"               . ?i)
                           ("/Sent Mail"   . ?s)
                           ("/Trash"       . ?t)
                           ("/All Mail"    . ?a)))
                  (setq mu4e-get-mail-command "offlineimap")
                  (setq message-send-mail-function 'smtpmail-send-it
                        smtpmail-stream-type 'starttls
                        smtpmail-default-smtp-server "smtp.gmail.com"
                        smtpmail-smtp-server "smtp.gmail.com"
                        smtpmail-smtp-service 587)))
