;;; app-x/email/config.el -*- lexical-binding: t; -*-
;;
;; Email and Git Configuration
;;
;; This file handles:
;; - SMTP email setup (QQ mail)
;; - GPG signing configuration
;; - Magit integration
;; - Email-based workflows
;;
;; Security features:
;; - Encrypted email sending
;; - Signed Git commits
;; - Secure credential storage
;;
;; See README.org for setup instructions.

(when IS-MAC
  (if (featurep! +gmail) (load! "+gmail"))
  (if (featurep! +notmuch) (load! "+notmuch"))
  (if (featurep! +gnus) (load! "+gnus")))

(defun my-setup-qq-email ()
  (setq send-mail-function 'smtpmail-send-it)
  (setq smtpmail-smtp-server "smtp.qq.com")
  (setq smtpmail-smtp-user "linuxing3@qq.com")
  (setq smtpmail-smtp-service 465)
  (setq smtpmail-stream-type 'starttls)
  "Setup qq email smtp account")

(my-setup-qq-email)
