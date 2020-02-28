;;; c:/Users/Administrator/.doom.d/utils.el -*- lexical-binding: t; -*-

;;;###autoload
(defun os-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (expand-file-name path "C:/Users/Administrator")
    (expand-file-name path "/home/linuxing3")))

;;;###autoload
(defun dropbox-path (path)
  "Prepend drive label to PATH."
  (if IS-WINDOWS
      (concat "D:/Dropbox/" path)
    (concat "/home/linuxing3/Dropbox/" path)))

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
(defun my-development-environment ()
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


(defun run-bash ()
  (interactive)
  (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe"))
    (shell "*bash*")))

(defun run-cmdexe ()
  (interactive)
  (let ((shell-file-name "cmd.exe"))
    (shell "*cmd.exe*")))

(defun run-powershell ()
  "Run powershell"
  (interactive)
  (async-shell-command "c:/windows/system32/WindowsPowerShell/v1.0/powershell.exe -Command -"
                       nil
                       nil))


(defun msbuild-2017-x86-setup ()
  "Set enviorment variables to load Microsoft Visual C++ Compiler (MSVC 32 bits)"
  (interactive)
  (message "Setting 32 bits MSVC building tools.")
  (setenv "PATH" msbuild-old-path-var)
  (setenv "INCLUDE"
          (concat
           "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/ATLMFC/include"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/include"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.6.1/include/um"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.16299.0/ucrt"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.16299.0/shared"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.16299.0/um"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.16299.0/winrt"
           ))

  (setenv "LIB"
          (concat
           "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/ATLMFC/lib/x86"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/lib/x86"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.6.1/lib/um/x86"
           ";" "C:/Program Files (x86)/Windows Kits/10/lib/10.0.16299.0/ucrt/x86"
           ";" "C:/Program Files (x86)/Windows Kits/10/lib/10.0.16299.0/um/x86"
           ))

  (setenv "LIBPATH"
          (concat
           "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/ATLMFC/lib/x86"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/lib/x86"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/lib/x86/store/references"
           ";" "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/10.0.16299.0"
           ";" "C:/Program Files (x86)/Windows Kits/10/References/10.0.16299.0"
           ";" "C:/Windows/Microsoft.NET/Framework/v4.0.30319"
           ))

  (setenv "PATH"
          (concat
           (getenv "PATH")
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.12.25827/bin/HostX86/x86"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/IDE/VC/VCPackages"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/IDE/CommonExtensions/Microsoft/TestWindow"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/MSBuild/15.0/bin/Roslyn"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Team Tools/Performance Tools"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/Shared/Common/VSPerfCollectionTools/"
           ";" "C:/Program Files (x86)/Microsoft SDKs/Windows/v10.0A/bin/NETFX 4.6.1 Tools/"
           ";" "C:/Program Files (x86)/Microsoft SDKs/F#/4.1/Framework/v4.0/"
           ";" "C:/Program Files (x86)/Windows Kits/10/bin/x86"
           ";" "C:/Program Files (x86)/Windows Kits/10/bin/10.0.16299.0/x86"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community//MSBuild/15.0/bin"
           ";" "C:/Windows/Microsoft.NET/Framework/v4.0.30319"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/IDE/"
           ";" "C:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/Tools/"
           )))
