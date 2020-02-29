;;; private/writer/+research.el -*- lexical-binding: t; -*-
;;;

(def-package! org-ref)
(def-package! org-noter)

(add-to-list 'org-latex-classes
             '("elsarticle"
               "\\documentclass{elsarticle}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
                 '("mimosis"
                   "\\documentclass{mimosis}
 [NO-DEFAULT-PACKAGES]
 [PACKAGES]
 [EXTRA]
\\newcommand{\\mboxparagraph}[1]{\\paragraph{#1}\\mbox{}\\\\}
\\newcommand{\\mboxsubparagraph}[1]{\\subparagraph{#1}\\mbox{}\\\\}"
                   ("\\chapter{%s}" . "\\chapter*{%s}")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                   ("\\mboxparagraph{%s}" . "\\mboxparagraph*{%s}")
                   ("\\mboxsubparagraph{%s}" . "\\mboxsubparagraph*{%s}")))

(after! org-ref
  ;; see org-ref for use of these variables
  (setq org-ref-default-bibliography '("~/Dropbox/shared/bibtex/publications.bib")
        org-ref-pdf-directory "~/Dropbox/shared/papers/"
        org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename
        bibtex-completion-pdf-field "file"
        org-latex-prefer-user-labels t
        org-ref-default-citation-link "parencite"
        ;; bibtex-dialect 'biblatex
        )

  (defun org-ref-open-pdf-at-point-in-emacs ()
    "Open the pdf for bibtex key under point if it exists."
    (interactive)
    (let* ((results (org-ref-get-bibtex-key-and-file))
           (key (car results))
           (pdf-file (funcall org-ref-get-pdf-filename-function key)))
      (if (file-exists-p pdf-file)
          (find-file-other-window pdf-file)
        (message "no pdf found for %s" key))))

  ;; https://github.com/jkitchin/org-ref/issues/597
  (defun org-ref-grep-pdf (&optional _candidate)
    "Search pdf files of marked CANDIDATEs."
    (interactive)
    (let ((keys (helm-marked-candidates))
          (get-pdf-function org-ref-get-pdf-filename-function))
      (helm-do-pdfgrep-1
       (-remove (lambda (pdf)
                  (string= pdf ""))
                (mapcar (lambda (key)
                          (funcall get-pdf-function key))
                        keys))))))
