;;; private/javascript/autoloads.el -*- lexical-binding: t; -*-
;;; Commentary:
;;;
;;; Evaluation functions for the `nodejs-repl' package.  Written on a stormy
;;; night between days of node hacking.

;;; Code:
(provide 'nodejs-repl-eval)

(require 'dash)
(require 'cl)
(require 'js2-mode)
(require 'nodejs-repl)

;;;###autoload
 (defun my-js2-mode-hook ()
  (progn
    ;; (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc-snippet)
    ;; (defalias 'docf 'js-doc-insert-function-doc-snippet "Document this function")
    ;; (define-key js2-mode-map "@" 'js-doc-insert-tag)
    ;; (defalias 'doct 'js-doc-insert-tag "Document this tag")
    ;; (global-set-key (kbd "M-s i") 'my-counsel-imenu)
    ;; (modify-syntax-entry ?_ "w")
    (which-function-mode t)
    (setq imenu-create-index-function 'my-js2-imenu-make-index)

    ;; (setq mode-name "JS2")
    ;; (define-key js2-mode-map   (kbd "s-.") 'company-tern)
    (spacemacs/toggle-syntax-checking-on)
    (setq forward-sexp-function nil)
    (set (make-local-variable 'semantic-mode) nil)))

;; TODO: defmacro to generate similar functions
;;;###autoload
(defun +javascript|setup-gatsby-project ()
  "Search the current buffer's parent directories for `src`.
If it's found, then generate ctags."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "src"))
            (path (expand-file-name "src" root)))
      (progn
        (with-dir root
                  (shell-command "ctags -eR src/"))
        (when doom-debug-mode
          (message "Found Gatsby Project with %s" path)))
    (when doom-debug-mode
      (message "Not found src dir in %s" root))))

;;;###autoload
(defun +javascript|setup-vue-cli-project ()
  "Search the current buffer's config files for `vue-cli`.
If it's found, then generate ctags."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "vue.config.js"))
            (path (expand-file-name "src" root)))
      (progn
        (with-dir root
                  (shell-command "ctags -eR src/"))
        (when doom-debug-mode
          (message "Found Vue-cli Project %s" path)))
    (when doom-debug-mode
      (message "Not found src dir in %s" root))))

;;;###autoload
(defun +javascript|setup-netlify-project ()
  "Search the current buffer's parent directories for `netlify`.
If it's found, then generate ctags."
  (if-let* ((root (locate-dominating-file
                   (or (buffer-file-name) default-directory)
                   "src/cms"))
            (path (expand-file-name "src/cms" root)))
      (progn
        (with-dir root
                  (shell-command "ctags -eR src/"))
        (when doom-debug-mode
          (message "Found Netlify Project %s" path)))
    (when doom-debug-mode
      (message "Not found src dir in %s" root))))

;;;###autoload
(defun nodejs-repl--sanitize-code (text)
  "Avoid conflicts with REPL special constructs: _ and .command"
  (->> text
    ;; obj EOL .fn() => obj. EOL fn() (while also removing "// comments")
    ;; If there is a chained call on a new line, move the dot to the previous line;
    ;; the repl executes lines eagerly and interprets " .something" as a REPL command
    (replace-regexp-in-string "\\(//.*\\)?\n\\(\\s-*\\)\\.\\(\\w+\\)" ".\n\\2\\3")
    ;; Replace _ with __ because underscore is a special thing in the REPL
    (replace-regexp-in-string "\\_<_\\." "__.")
    ;; Replace var _ = require ... with var __ = ...
    (replace-regexp-in-string "var\\s-+_\\s-+=" "var __ =")
    ))

;;;###autoload
(defun nodejs-repl-eval-region (start end)
  "Evaluate the region specified by `START' and `END'."
  (let ((proc (get-process nodejs-repl-process-name)))
    (comint-simple-send proc
                        (nodejs-repl--sanitize-code
                         (buffer-substring-no-properties start end)))))


;;;###autoload
(defun nodejs-repl-eval-node (node)
  "Evaluate `NODE', a `js2-mode' node."
  (let ((beg (js2-node-abs-pos node))
        (end (js2-node-abs-end node)))
    (nodejs-repl-eval-region beg end)))

;;;###autoload
(defun nodejs-repl--find-current-or-prev-node (pos &optional include-comments)
  "Locate the first node before `POS'.  Return a node or nil.

If `INCLUDE-COMMENTS' is set to t, then comments are considered
valid nodes.  This is stupid, don't do it."
  (let ((node (js2-node-at-point pos (not include-comments))))
    (if (or (null node)
            (js2-ast-root-p node))
        (unless (= 0 pos)
          (nodejs-repl--find-current-or-prev-node (1- pos) include-comments))
      node)))

;;;###autoload
(defun nodejs-repl-eval-function ()
  "Evaluate the current or previous function."
  (interactive)
  (let* ((fn-above-node (lambda (node)
                         (js2-mode-function-at-point (js2-node-abs-pos node))))
        (fn (funcall fn-above-node
             (nodejs-repl--find-current-or-prev-node
              (point) (lambda (node)
                        (not (null (funcall fn-above-node node))))))))
    (unless (null fn)
      (nodejs-repl-eval-node fn))))

;;;###autoload
(defun nodejs-repl-eval-first-stmt (pos)
  "Evaluate the first statement found from `POS' by `js2-mode'.

If this statement is a block statement, its first parent
statement is found.  This will be either a function declaration,
function call, or assignment statement."
  (let ((node (js2-mode-find-first-stmt (nodejs-repl--find-current-or-prev-node pos))))
    (cond
     ((js2-block-node-p node) (nodejs-repl-eval-node (js2-node-parent-stmt node)))
     ((not (null node)) (nodejs-repl-eval-node node)))))

;;;###autoload
(defun nodejs-repl-eval-dwim ()
  "Heuristic evaluation of JS code in a NodeJS repl.

Evaluates the region, if active, or the first statement found at
or prior to the point.

If the point is at the end of a line, evaluation is done from one
character prior.  In many cases, this will be a semicolon and will
change what is evaluated to the statement on the current line."
  (interactive)
  (cond
   ((use-region-p) (nodejs-repl-eval-region (region-beginning) (region-end)))
   ((= (line-end-position) (point)) (nodejs-repl-eval-first-stmt (1- (point))))
   (t (nodejs-repl-eval-first-stmt (point)))))

;;;###autoload
(defun nodejs-repl-eval-buffer (&optional buffer)
  "Evaluate the current buffer or the one given as `BUFFER'.

`BUFFER' should be a string or buffer."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (nodejs-repl-eval-region (point-min) (point-max)))))

;;;;; js2-mode 增强功能

;;;###autoload
 (defun my-js2-which-function ()

  (setq imenu--index-alist nil)
  (which-function-mode t)
  (which-function))
;;;###autoload
 (defun my-js2-imenu-make-index ()
  (interactive)
  (save-excursion
    ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
    (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("test" "\\s-*test\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                               ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                               ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                               ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                               ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                               ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                               ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                               ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                               ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^var[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                               ("Function" "^[ \t]*\\([^while|for ][a-zA-Z0-9_$]*\\)[ \t]*([a-zA-Z0-9_$,/\\* ]*)[ \t]*" 1)
                               ("Function" "^[ \t]*static[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*()[ \t]*{" 1)
                               ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                               ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                               ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                               ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))


