;;; autoload/fs.el -*- lexical-bindings: t; -*-

(require 'dash)
(require 'dash-functional)
(require 's)

;;; nil!
;;;###autoload
(defmacro nil! (var)
  "e.g.
(nil! x)"
  `(setq ,var nil))

;; Create Clojure def, defn and fn special forms

;;; fn
;;;###autoload
(defmacro fn (args body)
  "e.g.
ELISP-> (fn (x) (* x x))
(lambda
  (x)
  (* x x))
ELISP-> (mapcar (fn (x) (* x x)) '(1 2 3 4 5))
"
 `(lambda ,args ,body))


;;; def
;;;###autoload
(defmacro def (name value)
  "e.g.
(def x 1000)
"
   `(setq ,name ,value))


;;; defn
;;;###autoload
(defmacro defn (name args body)
  "e.g.
ELISP> (defn f (x y z) (+ (* 3 x) (* -4 y) (* 5 z)))
f
ELISP> (f 4 5 6)
22
"
   `(defun ,name ,args ,body))


;;; Clojure like macros

;;;###autoload
(defun foldl (f acc xss)
  (if (null xss)
      acc
      (foldl f (funcall f acc (car xss)) (cdr xss))))

;;;###autoload
(defun pass-result (x sexp)
  (if (listp sexp)
      `(,(car sexp) ,x ,@(cdr sexp))
      `(,sexp ,x)))

;;;###autoload
(defmacro -> (x &rest exprs)
  (foldl #'pass-result x exprs))

;;;###autoload
(defun pass-result-last (x sexp)
  (if (listp sexp)
    `(,(car sexp) ,@(cdr sexp) ,x)
    `(,sexp ,x)))

;;;###autoload
(defmacro --> (x &rest exprs)
  (foldl #'pass-result-last x exprs))

;;;###autoload
(defun replace (targ subst lst)
  (if (null lst)
      '()
    (let ((hd (car lst))
      (tl (cdr lst)))
      (if (equal targ hd)
      (cons subst (replace targ subst tl))
    (cons (if (listp hd) (replace targ subst hd) hd)
          (replace targ subst tl))))))

;;;###autoload
(defun pass-result-subst (x sexp)
  (if (listp sexp)
     (replace '$ x sexp)
    `(,sexp ,x)))

;;;###autoload
(defmacro $-> (x &rest exprs)
  "Thread-Substitute Macro"
  (foldl #'pass-result-subst x exprs))

;;;###autoload
(defmacro letc (bindings &rest body)
  "Clojure Let"
  `(let*
       ,(plist->alist bindings)
     ,@body))

;; Create Scheme Syntax define

;;;###autoload
(defmacro define (args body)
 (if (listp args)
     `(defun ,(car args) ,(cdr args) ,body)
     `(setq  ,args ,body)))
;; Rebind Elisp functions

;;; Bind new names to existing elisp functions in order to create user friendly
;;; identifiers, for example, bind the symbol map to the function mapcar.

;;;###autoload
(defmacro rebindfun (new-name old-name)
  "e.g.
(rebindfun 'map #'mapcar)
"
   `(setf (symbol-function ,new-name) ,old-name))

;; Convert Infix Operator to prefix operator

;;;###autoload
(defmacro $ (a op b)
  "e.g
ELISP> ($ 1 < 10)
t
"
  `(,op ,a ,b))


;; Debug injection

;;;###autoload
(defmacro $debug (func &rest params)
  "e.g
ELISP> ($debug + 10 ($debug * 10 30))
(* 10 30) = 300
(+ 10 ($debug * 10 30)) = 310
"
  `(let
      ((__r (,func ,@params)))
       (progn
     (print (format "%s = %s"
      (quote (,func ,@params))
       __r))
     __r)))

;;; os-path

;;;###autoload
(defun os-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
    (expand-file-name path "c:/Users/Wjb")
    (expand-file-name path "/Users/linuxing3")))

;;; with-dir

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
(defmacro makeAbrevFun (name val)
  `(defun ,(eval name) () (insert ,val)))

;;;###autoload
(defmacro define-doom-function (name &optional FORMS)
  "Define a function to new doom function."
  `(defun (intern (format "doom-x-%s" name)) ()
	 "Define new function with doom- prefix"
	 (interactive)
	 (progn
	   (insert "New function Created...")
	   ,@FORMS
	   (insert "New function Created...")
	   ))
  )
;; (macroexpand '(define-doom-dir define-keys))

;;; with-face
;;;###autoload
(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

;;; define-keys

;;;###autoload
(defun define-keys (keymap &rest pairs)
  "Define alternating key-def PAIRS for KEYMAP."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (define-key keymap key def))))

;;; global-set-keys

;;;###autoload
(defun global-set-keys (&rest pairs)
  "Set alternating key-def PAIRS globally."
  (-each
      (-partition 2 pairs)
    (-lambda ((key def))
      (global-set-key key def))))

;;; line breaks

;;;###autoload
(defun my-line-breaks ()
  " I would like to able to 'pretty-print' the file to span multiple lines to
make it more readable
The 1st no. represents the no. of csv values in the next section and so on
e.g.
3,1,2,3,3,4,5,6
would be converted to:
3,1,2,3
3,4,5,6
"
  (interactive)
  (while (search-forward "," nil t
                         (1+ (string-to-number (thing-at-point 'word))))
    (delete-char -1)
    (insert "\n")))

;; adjust-major-mode-keymap-with-evil

;;;###autoload
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

;;;###autoload
(defun load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
  (load-file f)))

;;;###autoload
(defun babel-load-if-exists (f)
  "babel load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
  (org-babel-load-file f)))

;;;###autoload
(defun my-open-in-external-app ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference. Version 2016-10-15"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))  $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                        (start-process "" nil "xdg-open" $fpath))) $file-list))))))

;;;###autoload
(defun my-open-in-terminal ()
  "Open the current dir in a new terminal window. Version 2018-12-07"
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (message "Microsoft Windows not supported. File a bug report or pull request."))
   ((string-equal system-type "darwin")
    (let ((process-connection-type nil))
      (start-process "" nil "/Applications/Utilities/Terminal.app/Contents/MacOS/Terminal" default-directory)))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil))
      (start-process "" nil "x-terminal-emulator"
                     (concat "--working-directory=" default-directory))))))

;;;###autoload
(defun my-open-in-safari ()
  "Open the current file or `dired' marked files in Mac's Safari browser.
If the file is not saved, save it first. Version 2018-02-26"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (when (buffer-modified-p )
             (save-buffer))
           (shell-command
            (format "open -a Safari.app \"%s\"" $fpath))) $file-list))))))

;;;###autoload
(defun my-show-kill-ring ()
  "Insert all `kill-ring' content in a new buffer named *copy history*. Version 2018-10-05"
  (interactive)
  (let (($buf (generate-new-buffer "*copy history*")))
    (progn
      (switch-to-buffer $buf)
      (funcall 'fundamental-mode)
      (setq buffer-offer-save t)
      (dolist (x kill-ring )
        (insert x "\n\u000cttt\n\n"))
      (goto-char (point-min)))
    (when (fboundp 'xah-show-formfeed-as-line)
      (xah-show-formfeed-as-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fs.el ends here
