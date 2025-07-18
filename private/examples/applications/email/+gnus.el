;;; app-x/email/config.el -*- lexical-binding: t; -*-

;; Set this in ~/.gnus
(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
	       (nnimap-server-port "imaps")
	       (nnimap-stream ssl)))

(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; set this in ~/.authinfo
;; machine imap.gmail.com login <USER> password <APP-PASSWORD> port imaps
;; machine smtp.gmail.com login <USER> password <APP-PASSWORD> port 587
