;;; private/writer/autoload.el -*- lexical-binding: t; -*-

(defvar my-font-list nil "A list of fonts for `my-cycle-font' to cycle from.")

(setq my-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Yahei Consolas Hybrid-16"
          "Microsoft Yahei-16"
          "Fira Code-16"
          "Courier-16"
          "Lucida Console-16"
          "Segoe UI Symbol-16"
          "Lucida Sans Unicode-16"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "DejaVu Sans Mono-16"
          "DejaVu Sans-16"
          "Symbola-14"
          ))
       ((string-equal system-type "darwin") ; Mac
        '(
          "Fira Code-16"
          "Monaco-16"
          "Courier-16"
          "Menlo-16"))))

;;;###autoload
(defun my-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.3))
  (redraw-frame (selected-frame)))

;;;###autoload
(defun my-new-frame-style ()
  "Customizing the style of new frame or window.
"
  (progn
    (if (display-graphic-p)
        (progn
          (setq initial-frame-alist
                '(
                  (tool-bar-lines . 0)
                  (width . 106) ; chars
                  (height . 60) ; lines
                  (background-color . "honeydew")
                  ;;
                  ))
          (setq default-frame-alist
                '(
                  (tool-bar-lines . 0)
                  (width . 106)
                  (height . 60)
                  (background-color . "honeydew")
                  ;;
                  )))
      (progn
        (setq initial-frame-alist
              '(
                (tool-bar-lines . 0)))
        (setq default-frame-alist
              '(
                (tool-bar-lines . 0)))))
    ))

;;;###autoload
(defun my-cycle-font ()
  "cycle multiple font"
  (cond
   ((IS-WINDOWS (set-frame-font "Yahei Consolas Hybrid" t t))
   (IS-MAC (set-frame-font "Fira Code" t t)))))

;;;###autoload
(defun my-cycle-font (@n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `my-font-list' .
If @n is 1, cycle forward.
If @n is -1, cycle backward.
See also `my-cycle-font-next', `my-cycle-font-previous'.

URL `http://ergoemacs.org/emacs/emacs_switching_fonts.html'
Version 2015-09-21"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let ($fontToUse $stateBefore $stateAfter )
    (setq $stateBefore (if (get 'my-cycle-font 'state) (get 'my-cycle-font 'state) 0))
    (setq $stateAfter (% (+ $stateBefore (length my-font-list) @n) (length my-font-list)))
    (setq $fontToUse (nth $stateAfter my-font-list))
    (set-frame-font $fontToUse t)
    ;; (set-frame-parameter nil 'font $fontToUse)
    (message "Current font is: %s" $fontToUse )
    (put 'my-cycle-font 'state $stateAfter)))

;;;###autoload
(defun my-cycle-font-next ()
  "Switch to the next font, in current window.
See `my-cycle-font'."
  (interactive)
  (my-cycle-font 1))

;;;###autoload
(defun my-cycle-font-previous ()
  "Switch to the previous font, in current window.
See `my-cycle-font'."
  (interactive)
  (my-cycle-font -1))
