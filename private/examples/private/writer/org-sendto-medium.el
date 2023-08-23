     ;;; org-sendto-medium.el --- Export articles in Org-Mode to Medium
     ;; Copyright (C) 2016 Haoyang Xu

     ;; Author: Haoyang Xu <haoyang@expoundite.net>
     ;; Created: 22 Mar 2016
     ;; Version: 0.1
     ;; Package-Requires: ((org "8.0")(emacs "24.1"))

     ;; Keywords: comm, processes
     ;; Homepage: https://github.com/celadevra/org-sendto-medium

     ;; This file is not part of GNU Emacs.

     ;;; Commentary:

     ;; This package provides a function to publish Org-Mode files to Medium.com.

     ;;; Code:

     (require 'url)
     (require 'json)

      (defvar org-medium-apibegin "https://api.medium.com/v1"
        "Beginning of the Medium API endpoints. The part from 'https://' to the end of version indicator.")

      (defvar org-medium-apiuser (concat org-medium-apibegin "/me")
        "API Endpoint for obtaining user information, such as ID.")

      (defcustom org-medium-integration-token nil
        "Self-issued token for authentication with Medium. You can generate yours at https://medium.com/me/settings"
        :group 'org-medium
        :type 'string)

      (defvar org-medium-author-id nil
        "Author id returned by Medium API given correct token.")

      (defcustom org-medium-default-license "all-rights-reserved"
        "Default license for posts published."
        :group 'org-medium
        :type '(choice
                (string :tag "all-rights-reserved" "all-rights-reserved")
                (string :tag "cc-40-by" "cc-40-by")
                (string :tag "cc-40-by-sa" "cc-40-by-sa")
                (string :tag "cc-40-by-nd" "cc-40-by-nd")
                (string :tag "cc-40-by-nc" "cc-40-by-nc")
                (string :tag "cc-40-by-nc-nd" "cc-40-by-nc-nd")
                (string :tag "cc-40-by-nc-sa" "cc-40-by-nc-sa")
                (string :tag "cc-40-zero" "cc-40-zero")
                (string :tag "public-domain" "public-domain")))

      (defcustom org-medium-default-visibility "public"
        "Default visibility of posts published. Can be one status among the 3 below:
      public, unlisted, or draft."
        :group 'org-medium
        :type '(choice
                (string :tag "Public" "public")
                (string :tag "Unlisted" "unlisted")
                (string :tag "Draft" "draft")))

      (defun org-medium-test-token ()
        "Test if the integration token for medium is present. If not, ask the user to get one and open the url for user."
        (if (or (not org-medium-integration-token) (string= "" org-medium-integration-token))
            (progn
              (if (y-or-n-p "Your integration token is not set, take you to medium so you can get one? ")
                  (browse-url-default-browser "https://medium.com/me/settings"))
              (generate-new-buffer "*Instructions*")
              (switch-to-buffer-other-window "*Instructions*")
              (insert "Scroll to the bottom of your Medium settings page, find heading \"integration tokens\".\n
      In the text box below, input an identifier such as \"my emacs\", \n
      and hit the \"Get integration token\" button, copy the generated\n
      token and paste it in the minibuffer.")
              (let ((x (read-string "Paste your integration token here: ")))
                (customize-save-variable 'org-medium-integration-token (eval x)))
              (message "Integration token saved.")
              (kill-buffer "*Instructions*"))
          (message "Integration token found.")))

      (defun org-medium-get-authorid ()
        "Obtain author information from Medium and return the id for later use"
        (progn
          (org-medium-test-token)
          (org-medium-me-query)))

      (defun org-medium-me-query ()
        "Query Medium for user information."
        (let* ((url-request-method "GET")
               (auth-token (concat "Bearer " org-medium-integration-token))
               (url-request-extra-headers
                `(("Content-Type" . "application/json")
                  ("Accept" . "application/json")
                  ("Authorization" . ,auth-token)
                  ("Accept-Charset" . "utf-8"))))
          (url-retrieve org-medium-apiuser 'org-medium-find-id)))

      (defun org-medium-find-id (status)
        "Parse JSON to extract required data from response."
        (if status ;something bad happens on the remote end
            (message "Medium returns error %s. Please try later." (car (plist-get status :error)))
          (progn
            (switch-to-buffer (current-buffer))
            (set-window-point (selected-window) (point-min))
            (search-forward-regexp "\"id\":\"\\([0-9abcdef]*\\)\"")
            (setq org-medium-author-id (current-word))
            (kill-buffer))))

      (defun org-medium-get-content (title)
        "Get generated html from Org's export buffer."
        (save-excursion
          (let ((buffer (org-html-export-as-html nil nil nil t '(:with-toc  nil))))
            (org-medium-process-html buffer title))))

      (defun org-medium-process-html (buffer title)
        "Sanitize buffer content so they are acceptable by Medium's API.
      Only tags such as <h1><h2><blockquote><p><figure><a><hr> and some
       emphases are accepted."
        (save-excursion
          (with-current-buffer buffer
	    (goto-char (point-min))
	    (insert (concat "<h1>" title "</h1>"))
            (let ((string (buffer-string)))
              (replace-regexp-in-string "\\\n" "" string)))))

      (defun org-medium-get-title ()
        "Get title from the #+TITLE keyword of current document."
        (save-excursion
          (goto-char (point-min))
          (search-forward-regexp "#\\+title:\\ *")
          (let ((beg (point))) (end-of-line) (buffer-substring-no-properties beg (point)))))

      (defun org-sendto-medium (&optional arg lic visib)
        "When called without arguments, publish your post to Medium with default settings.

        When called with universal argument, allow interactive selection of license and visibility.

      When called with LIC and/or VISIB arguments, send post request with customized arguments to alter publishing behaviour.

      Possible LIC values are:
      \"all-rights-reserved\"
      \"cc-40-by\"
      \"cc-40-by-sa\"
      \"cc-40-by-nc\"
      \"cc-40-by-nd\"
      \"cc-40-by-nc-nd\"
      \"cc-40-by-nc-sa\"
      \"cc-40-zero\"
      \"public-domain\"

      Possible VISIB values are \"public\" \"draft\" and \"unlisted\".
      "
        (interactive "P")
        (if (not (and org-medium-author-id (org-medium-test-token)))
            (setq org-medium-author-id (org-medium-get-authorid)))
        (let* ((url-request-method "POST")
               (auth-token (concat "Bearer " org-medium-integration-token))
               (url-request-extra-headers
                `(("Content-Type" . "application/json")
                  ("Accept" . "application/json")
                  ("Authorization" . ,auth-token)
                  ("Accept-Charset" . "utf-8")))
               (custom-params (equal arg '(4)))
               (title (org-medium-get-title))
               (content (org-medium-get-content title))
               (content-format "html")
               (license (if custom-params (org-medium-show-license-help)
                          (or lic org-medium-default-license)))
               (publish-status (if custom-params (org-medium-show-visibility-help)
                                 (or visib org-medium-default-visibility)))
               (url-request-data (json-encode-plist `(:title ,title
                                                             :contentFormat ,content-format
                                                             :content ,content
                                                             :publishStatus ,publish-status
                                                             :license ,license)))
               (url (concat org-medium-apibegin "/users/" org-medium-author-id "/posts")))
          (url-retrieve url (lambda (status) (switch-to-buffer (current-buffer)))))
        (kill-buffer (get-buffer "*Licenses*"))
        (kill-buffer (get-buffer "*Visibility*")))

     (defun org-medium-show-license-help ()
       "Helper function, show a buffer with possible licenses, let user choose, and return the license value."
       ()
       (generate-new-buffer "*Licenses*")
       (switch-to-buffer-other-window "*Licenses*")
       (insert "Possible choices:
     All rights reserved [a]
     CC-BY [b]         CC-BY-SA [s]
     CC-BY-NC [c]      CC-BY-ND [d]
     CC-BY-NC-SA [n]   CC-BY-NC-ND [y]
     CC-0 [0]
     Public Domain [o]
     ")
       (let ((x (read-char-choice "Select a license: " (append "abscdny0o" nil))))
         (cond ((equal x 97) "all-rights-reserved")
               ((equal x 98) "cc-40-by")
               ((equal x 115) "cc-40-by-sa")
               ((equal x 99) "cc-40-by-nc")
               ((equal x 100) "cc-40-by-nd")
               ((equal x 110) "cc-40-by-nc-sa")
               ((equal x 121) "cc-40-by-nc-nd")
               ((equal x 48) "cc-zero")
               ((equal x 111) "public-domain"))))

     (defun org-medium-show-visibility-help ()
       "Helper function, show a buffer with possible visibility choices for post, let user choose, and return the visibility value."
       ()
       (generate-new-buffer "*Visibility*")
       (switch-to-buffer-other-window "*Visibility*")
       (insert "Possible choices:
     Public [P]
     Draft [D]
     Unlisted [U]")
       (case (read-char-choice "Select visibility: " (append "PDU" nil))
         (80 "published")
         (68 "draft")
         (85 "unlisted")))

     (provide 'org-sendto-medium)
     ;;; org-sendto-medium.el ends here
