;;; +mac.el -*- lexical-binding: t; -*-
;;;###autoload
(defun open-in-iterm ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
)

;;;###autoload
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

;;;###autoload
(defun iterm-repeat-last-command ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    "  activate\n"
    "    tell current session of current window\n"
    "      tell application \"System Events\" to keystroke (ASCII character 30)\n" ;; up arrow
    "      tell application \"System Events\" to key code 36\n" ;; return
    "    end tell\n"
    "end tell\n")))
