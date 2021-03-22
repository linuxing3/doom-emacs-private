;;; autoload/window.el -*- lexical-binding: t; -*-
(require 'dash)
(require 'dash-functional)
(require 's)

;;;###autoload
(defun +my-get-buffer-tree (wintree)
  "Taken from evil-window.el: Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'+my-get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

;;;###autoload
(defun +my-restore-window-tree (win tree)
  "Taken from evil-window.el: Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (+my-restore-window-tree win (cadr tree))
      (+my-restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

;;;###autoload
(defun +my-window-swap (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
+my/window-move-* (e.g. `+my/window-move-far-left')"
  (require 'windmove)
  (when (window-dedicated-p)
    (user-error "Cannot swap a dedicated window"))
  (let* ((this-window (selected-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (window-dedicated-p this-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (pcase direction
                   ('left  #'+my/window-move-far-left)
                   ('right #'+my/window-move-far-right)
                   ('up    #'+my/window-move-very-top)
                   ('down  #'+my/window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil
                            (pcase direction
                              ('up 'above)
                              ('down 'below)
                              (_ direction))))
        (with-selected-window that-window
          (switch-to-buffer (doom-fallback-buffer)))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +my/window-move-left () "See `+my-window-swap'"  (interactive) (+my-window-swap 'left))
;;;###autoload
(defun +my/window-move-right () "See `+my-window-swap'" (interactive) (+my-window-swap 'right))
;;;###autoload
(defun +my/window-move-up () "See `+my-window-swap'"    (interactive) (+my-window-swap 'up))
;;;###autoload
(defun +my/window-move-down () "See `+my-window-swap'"  (interactive) (+my-window-swap 'down))

;;;###autoload
(defun +my/window-move-very-top ()
  "Taken from evil-window.el: Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+my-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window)))
          (+my-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +my/window-move-far-left ()
  "Taken from evil-window.el: Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+my-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window-horizontally)))
          (+my-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +my/window-move-far-right ()
  "Taken from evil-window.el: Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+my-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window-horizontally)))
          (+my-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +my/window-move-very-bottom ()
  "Taken from evil-window.el: Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (+my-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window)))
          (+my-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

;;;###autoload
(defun +my/switch-to-last-window ()
  "Switch to the previously selected window, skipping any other window in between."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found."))
    (let ((frame (window-frame win)))
      (raise-frame frame)
      (select-frame frame)
      (select-window win))))

