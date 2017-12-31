;;; config.el --- kevin-org-layer layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: kevin <kevin.scnu@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

(defconst kevin-org-layer-packages
  '(
    (org :location built-in)
    org-pomodoro
    ))

(defun kevin-org-layer/post-init-org ()
  (with-eval-after-load 'org
    (progn
      (spacemacs|disable-company org-mode)
      (spacemacs/set-leader-keys-for-major-mode 'org-mode "," 'org-priority)
      (require 'org-compat)
      (require 'org)

      ;; config stuck project
      (setq org-stuck-projects
            '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

      (setq org-agenda-inhibit-startup t) ;; ~50x speedup
      (setq org-agenda-span 'day)
      (setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
      (setq org-agenda-window-setup 'current-window)
      (setq org-log-done t)

      ;; set keywords
      (setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                                (sequence "⚑ WAITING(w)" "|")
                                (sequence "|" "✘ CANCELED(c)")))

      ;; Change task state to STARTED when clocking in
      (setq org-clock-in-switch-to-state "STARTED")
      ;; Save clock data and notes in the LOGBOOK drawer
      (setq org-clock-into-drawer t)
      ;; Removes clocked tasks with 0:00 duration
      (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

      (setq org-tags-match-list-sublevels nil)

      ;; define the refile targets
      (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
      (setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-file-life (expand-file-name "life.org" org-agenda-dir))
      (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
      (setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
      (setq org-agenda-files (list org-agenda-dir))

      (with-eval-after-load 'org-agenda
        (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
        (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
          "." 'spacemacs/org-agenda-transient-state/body)
        )

      ;; appt
      (require 'appt)
      (setq appt-time-msg-list nil)    ;; clear existing appt list
      (setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
      (setq appt-message-warning-time '20)  ;; send first warning 15 minutes before appointment
      (setq appt-display-mode-line nil)   ;; don't show in the modeline
      (setq appt-display-format 'window)   ;; pass warnings to the designated window function
      (appt-activate 1)                ;; activate appointment notification
      (display-time)                   ;; activate time display
      (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
      (run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
      (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
      ;; notification app
      (setq appt-disp-window-function (function kevin/appt-notification))


      ;; the %i would copy the selected text into the template
      ;;http://www.howardism.org/Technical/Emacs/journaling-org.html
      ;;add multi-file journal
      (setq org-capture-templates
            '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Workspace")
               "* TODO [#B] %?\n  %i\n"
               :empty-lines 1)
              ("n" "notes" entry (file+headline org-agenda-file-note "Quick notes")
               "* %?\n  %i\n %U"
               :empty-lines 1)
              ("b" "Blog Ideas" entry (file+headline org-agenda-file-note "Blog Ideas")
               "* TODO [#B] %?\n  %i\n %U"
               :empty-lines 1)
              ("w" "work" entry (file+headline org-agenda-file-gtd "Danmaku")
               "* TODO [#A] %?\n  %i\n %U"
               :empty-lines 1)
              ("l" "life" entry (file+headline org-agenda-file-life "Lifespace")
               "* TODO [#C] %?\n  %i\n %a \n %U"
               :empty-lines 1)
              ("j" "Journal Entry"
               entry (file+datetree org-agenda-file-journal)
               "* %?"
               :empty-lines 1)))

      ;;An entry without a cookie is treated just like priority ' B '.
      ;;So when create new task, they are default 重要且紧急
      (setq org-agenda-custom-commands
            '(
              ("w" . "任务安排")
              ("wa" "重要且紧急的任务" tags-todo "+PRIORITY=\"A\"")
              ("wb" "重要且不紧急的任务" tags-todo "-Weekly-Monthly-Daily+PRIORITY=\"B\"")
              ("wc" "不重要且紧急的任务" tags-todo "+PRIORITY=\"C\"")
              ("b" "Blog" tags-todo "BLOG")
              ("p" . "项目安排")
              ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"danmu\"")
              ("pe" tags-todo "PROJECT+DREAM+CATEGORY=\"emacs\"")
              ("W" "Weekly Review"
               ((stuck "") ;; review stuck projects as designated by org-stuck-projects
                (tags-todo "PROJECT")
                ))))
      )))

(defun kevin-org-layer/post-init-org-pomodoro ()
  (progn
    (add-hook 'org-pomodoro-finished-hook '(lambda () (kevin/termianl-notification "Pomodoro Finished" "☕️ Have a break!")))
    (add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (kevin/termianl-notification "Short Break" "☕️ Ready to Go?")))
    (add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (kevin/termianl-notification "Long Break" "☕️ Ready to Go?")))
    ))


;;; packages.el ends here
