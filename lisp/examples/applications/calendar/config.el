;;; app-x/calendar/config.el -*- lexical-binding: t; -*-

(def-package! calfw
  :commands (cfw:open-calendar-buffer)
  :config
  (setq cfw:face-title              :foreground blue                     :bold bold :height 2.0 :inherit 'variable-pitch)
  (setq cfw:face-header             :foreground (doom-blend blue bg 0.8) :bold bold)
  (setq cfw:face-sunday             :foreground (doom-blend red bg 0.8)  :bold bold)
  (setq cfw:face-saturday           :foreground (doom-blend red bg 0.8)  :bold bold)
  (setq cfw:face-holiday            :foreground nil :background bg-alt   :bold bold)
  (setq cfw:face-grid               :foreground vertical-bar)
  (setq cfw:face-periods            :foreground yellow)
  (setq cfw:face-toolbar            :foreground nil :background nil)
  (setq cfw:face-toolbar-button-off :foreground base6                    :bold bold             :inherit 'variable-pitch)
  (setq cfw:face-toolbar-button-on  :foreground blue                     :bold bold             :inherit 'variable-pitch)
  (setq cfw:face-default-content    :foreground fg)
  (setq cfw:face-day-title          :foreground fg                       :bold bold)
  (setq cfw:face-today-title        :foreground bg  :background blue     :bold bold)
  (setq cfw:face-default-day                                             :bold bold)
  (setq cfw:face-today              :foreground nil :background nil      :bold bold)
  (setq cfw:face-annotation         :foreground violet)
  (setq cfw:face-disable            :foreground grey)
  (setq cfw:face-select             :background region))

(def-package! calfw-org
  :commands (cfw:open-org-calendar
             cfw:org-create-source
             cfw:open-org-calendar-withkevin
             my-open-calendar))
