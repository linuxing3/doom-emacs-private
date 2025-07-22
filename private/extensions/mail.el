;;; private/extensions/mail.el -*- lexical-binding: t; -*-
;; smtp settings
(require 'smtpmail-async)

(add-load-path! (expand-file-name
                 "~/.config/doom/private/mu4e"))

(setq smtpmail-smtp-user     "linuxing3@qq.com")
(setq smtpmail-smtp-server   "smtp.qq.com")
(setq smtpmail-smtp-service  465)
(setq smtpmail-stream-type  'ssl)

(setq send-mail-function #'smtpmail-send-it)
(setq message-send-mail-function #'smtpmail-send-it)

;; accounts
(after! mu4e
  ;; FIXME: we use nixos agenix instead of pass-store
  (when (modulep! :tools pass)
    (add-hook 'async-smtpmail-before-send-hook #'auth-source-pass-enable))

  ;; auto update email
  (setq mu4e-update-interval 60)
  ;; use org to compose html email
  ;; (setq +mu4e-compose-org-msg-toggle-next nil)

  (set-email-account! "qq.com"
                      '((mu4e-sent-folder       . "/qq/Sent Messages")
                        (mu4e-drafts-folder     . "/qq/Drafts")
                        (mu4e-trash-folder      . "/qq/Trash")
                        (mu4e-refile-folder     . "/qq/All Mail")

                        (smtpmail-smtp-user     . "linuxing3@qq.com")
                        (smtpmail-smtp-server   . "smtp.qq.com")
                        (smtpmail-smtp-service  . 465)
                        (smtpmail-stream-type   . ssl)

                        (send-mail-function . smtpmail-send-it)
                        (message-send-mail-function . smtpmail-send-it)

                        (message-signature . "---\nYours truly\nXing Wenju\nMinister\nChinese Embassy in Brazil"))
                      t)

  (set-email-account! "gamil.com"
                      '((mu4e-sent-folder       . "/gmail/Sent Messages")
                        (mu4e-drafts-folder     . "/gmail/Drafts")
                        (mu4e-trash-folder      . "/gmail/Trash")
                        (mu4e-refile-folder     . "/gmail/All Mail")

                        (smtpmail-smtp-user     . "xingwenju@gmail.com")
                        (smtpmail-smtp-server   . "smtp.gmail.com")
                        (smtpmail-smtp-service  . 587)
                        (smtpmail-stream-type   . starttls)

                        (send-mail-function . smtpmail-send-it)
                        (message-send-mail-function . smtpmail-send-it)

                        (message-signature . "---\nYours truly\nXing Wenju\nMinister\nChinese Embassy in Brazil"))
                      nil)

  )

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
