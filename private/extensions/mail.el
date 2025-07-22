;;; private/extensions/mail.el -*- lexical-binding: t; -*-

(add-load-path! (expand-file-name
                 "~/.config/doom/private/mu4e"))
;; accounts
(after! mu4e
  ;; FIXME: we use nixos agenix instead of pass-store
  ;; (when (modulep! :tools pass)
  ;;   (add-hook 'async-smtpmail-before-send-hook #'auth-source-pass-enable))

  ;; smtp settings
  (require 'smtpmail-async)
  (setq smtpmail-default-smtp-server "smtp.qq.com")
  (setq smtpmail-smtp-user      "linuxing3@qq.com")
  (setq smtpmail-smtp-server     "smtp.qq.com")
  (setq smtpmail-smtp-service  465)

  (setq sendmail-program (executable-find "msmtp")
        send-mail-function #'async-smtpmail-send-it
        message-send-mail-function #'async-smtpmail-send-it)

  (setq mu4e-update-interval 60)

  (setq +mu4e-compose-org-msg-toggle-next nil)

  ;; Each path is relative to the path of the maildir you passed to mu
  (set-email-account! "qq.com"
                      '((mu4e-sent-folder       . "/qq/Sent Messages")
                        (mu4e-drafts-folder     . "/qq/Drafts")
                        (mu4e-trash-folder      . "/qq/Trash")
                        (mu4e-refile-folder     . "/qq/All Mail")

                        (smtpmail-smtp-user     . "linuxing3@qq.com")
                        (smtpmail-smtp-server   . "smtp.qq.com")
                        (smtpmail-smtp-service  . 465)
                        (smtpmail-stream-type   . ssl)
                        (smtpmail-servers-requiring-authorization . "smtp\\.qq\\.com")

                        (send-mail-function . async-smtpmail-send-it)
                        (message-send-mail-function . async-smtpmail-send-it)

                        (mu4e-compose-signature . "---\nYours truly\nXing Wenju\nMinister\nChinese Embassy in Brazil"))
                      t))

(setq +notmuch-sync-backend 'mbsync)
(after! notmuch
  (setq notmuch-show-log nil
        notmuch-hello-sections `(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-alltags)
        ;; To hide headers while composing an email
        notmuch-message-headers-visible nil))
(setq +notmuch-home-function (lambda () (notmuch-search "tag:inbox")))
(after! org-mime
  (setq org-mime-export-options '(:section-numbers nil
                                  :with-author nil
                                  :with-toc nil)))
