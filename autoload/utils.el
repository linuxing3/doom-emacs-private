;;; ~/.config/doom/autoload/utils.el -*- lexical-binding: t; -*-
;;;

;; Setting directories
(defvar home-directory ""
  "Home directory")

(defvar data-drive "")

(setq data-drive "/")

(if (equal nil (getenv "HOME"))
    (setq home-directory "~/")
  (setq home-directory (expand-file-name (getenv "HOME"))))

;;;###autoload
(defun os-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (expand-file-name path home-directory)
    (expand-file-name path home-directory)))

;;;###autoload
(defun dropbox-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (concat data-drive "/OneDrive/" path)
    (concat home-directory "/OneDrive/" path)))

;;;###autoload
(defun workspace-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (concat data-drive "/sources/" path)
    (concat home-directory "/sources/" path)))

;;;###autoload
(defmacro with-dir (DIR &rest FORMS)
  "Execute FORMS in DIR."
  (let ((orig-dir (gensym)))
    `(prog2
         (setq ,orig-dir default-directory)
         (progn (cd ,DIR) ,@FORMS)
       (cd ,orig-dir))))
;; (macroexpand '(with-dir "~/.emacs.d"))

;;;###autoload
(defun org-global-props (&optional property buffer)
  "Get the plists of global org properties of current buffer."
  (unless property (setq property "PROPERTY"))
  (with-current-buffer (or buffer (current-buffer))
    (org-element-map (org-element-parse-buffer) 'keyword (lambda (el) (when (string-match property (org-element-property :key el)) el)))))

;;;###autoload
(defun org-global-prop-value (key)
  "Get global org property KEY of current buffer."
  (org-element-property :value (car (org-global-props key))))

;;;###autoload
(defun linuxing3/development-environment ()
  ;; Ensure the babel load file type
  (with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
     '(
       (emacs-lisp . t)
       (python . t)
       (ein . t)
       (javacript . t)
       (typescript . t)
       (shell . t)
       (R . t)
       (ditaa . t)
       (plantuml . t)
       ))))

(defvar +linuxing3--scratch-frame nil)

;;;###autoload
(defun cleanup-scratch-frame (frame)
  (when (eq frame +linuxing3--scratch-frame)
    (with-selected-frame frame
      (setq doom-fallback-buffer-name (frame-parameter frame 'old-fallback-buffer))
      (remove-hook 'delete-frame-functions #'cleanup-scratch-frame))))

;;;###autoload
(defun open-scratch-frame (&optional fn)
  "Opens the org-capture window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (let* ((frame-title-format "")
         (preframe (cl-loop for frame in (frame-list)
                            if (equal (frame-parameter frame 'name) "scratch")
                            return frame))
         (frame (unless preframe
                  (make-frame `((name . "scratch")
                                (width . 120)
                                (height . 24)
                                (transient . t)
                                (internal-border-width . 10)
                                (left-fringe . 0)
                                (right-fringe . 0)
                                (undecorated . t)
                                ,(if IS-LINUX '(display . ":0")))))))
    (setq +linuxing3--scratch-frame (or frame posframe))
    (select-frame-set-input-focus +linuxing3--scratch-frame)
    (when frame
      (with-selected-frame frame
        (if fn
            (call-interactively fn)
          (with-current-buffer (switch-to-buffer "*scratch*")
            ;; (text-scale-set 2)
            (when (eq major-mode 'fundamental-mode)
              (emacs-lisp-mode)))
          (redisplay)
          (set-frame-parameter frame 'old-fallback-buffer doom-fallback-buffer-name)
          (setq doom-fallback-buffer-name "*scratch*")
          (add-hook 'delete-frame-functions #'cleanup-scratch-frame))))))
