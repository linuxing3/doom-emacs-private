;;; app-x/calendar/autoloads.el -*- lexical-binding: t; -*-

;;;###autoload
(defun cfw:open-org-calendar-with-cal1 ()
  (interactive)
  (let ((org-agenda-files '("~/Dropbox/org/calendar.org")))
    (call-interactively '+calendar/open-calendar)))

;;;###autoload
(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    (cfw:howm-create-source "Blue")  ; howm source
    (cfw:cal-create-source "Orange") ; diary source
    (cfw:ical-create-source "Moon" "~/Dropbox/shared/work.ics" "Gray")  ; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed") ; google calendar ICS
   )))
