(defun doom-project-workspace-switch ()
  "Switch to a workspace named after the project."
  (let ((project-name (projectile-project-name)))
    (+workspace-switch project-name t)
    (find-file (projectile-project-root))))

(after! projectile
  (setq +workspaces-on-switch-project-behavior t)
  ;; Automatically create/switch workspace when switching projects
  (setq projectile-switch-project-action #'doom-project-workspace-switch))
