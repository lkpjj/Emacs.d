;;; config.el --- kevin-better-defaults layer config file for Spacemacs.
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

;; personal config
(setq user-full-name "kevin")
(setq user-mail-address "kevin.scnu@gmail.com")
;; remove menu bar
(setq menu-bar-mode nil)
;; default directory
(setq-default default-directory "~/Code/gopath/src")

;; editor config
(setq-default fill-column 80)
;; (which-function-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
              ;; '((which-func-mode ("" which-func-format " "))))
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visi		dms           []*model.DMting a file)
(setq frame-title-format
      '("" " Kevin "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

;; helm config
;; move helm input in minibuffer
(setq helm-echo-input-in-header-line nil)
(remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;; hook
(add-hook 'dired-mode-hook 'my-dired-mode-hook)
(add-hook 'prog-mode-hook 'my-prog-mode-hook)
(add-hook 'after-save-hook 'revert-buffer-no-confirm)

;; ;; UI
;; (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
;;       evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
;;       evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
;;       evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
;;       evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
;;       evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

;; org
(defvar org-agenda-dir ""
  "gtd org files location")
(setq-default org-agenda-dir "~/Code/org-notes")
(setq org-agenda-files (list org-agenda-dir))

(add-hook 'org-pomodoro-finished-hook '(lambda () (notify-osx "Pomodoro Finished" "☕️ Have a break!")))
(add-hook 'org-pomodoro-short-break-finished-hook '(lambda () (notify-osx "Short Break" "☕️ Ready to Go?")))
(add-hook 'org-pomodoro-long-break-finished-hook '(lambda () (notify-osx "Long Break" "☕️ Ready to Go?")))

(spacemacs|disable-company org-mode)
;; (setq org-refile-targets
;;       '((nil :maxlevel . 4)
;;         (org-agenda-files :maxlevel . 4)))
;; config stuck project
(setq org-stuck-projects
      '("TODO={.+}/-DONE" nil nil "SCHEDULED:\\|DEADLINE:"))

(setq org-agenda-inhibit-startup t) ;; ~50x speedup
(setq org-agenda-span 'day)
(setq org-agenda-use-tag-inheritance nil) ;; 3-4x speedup
(setq org-agenda-window-setup 'current-window)
(setq org-log-done t)

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "SOMEDAY(S)" "|" "CANCELLED(c@/!)" "MEETING(m)" "PHONE(p)"))))

;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t) ;; Show the clocked-in task - if any - in the header line

(setq org-tags-match-list-sublevels nil)
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-files (list org-agenda-dir))

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
        ("s" "Code Snippet" entry
          (file org-agenda-file-code-snippet)
          "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
        ("w" "work" entry (file+headline org-agenda-file-gtd "Cocos2D-X")
          "* TODO [#A] %?\n  %i\n %U"
          :empty-lines 1)
        ("c" "Chrome" entry (file+headline org-agenda-file-note "Quick notes")
          "* TODO [#C] %?\n %(zilongshanren/retrieve-chrome-current-tab-url)\n %i\n %U"
          :empty-lines 1)
        ("l" "links" entry (file+headline org-agenda-file-note "Quick notes")
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
        ("pw" tags-todo "PROJECT+WORK+CATEGORY=\"cocos2d-x\"")
        ("pl" tags-todo "PROJECT+DREAM+CATEGORY=\"zilongshanren\"")
        ("W" "Weekly Review"
          ((stuck "") ;; review stuck projects as designated by org-stuck-projects
          (tags-todo "PROJECT") ;; review all projects (assuming you use todo keywords to designate projects)
          ))))

;; (require 'appt)
(setq appt-time-msg-list nil)    ;; clear existing appt list
(setq appt-display-interval '5)  ;; warn every 5 minutes from t - appt-message-warning-time
(setq
 appt-message-warning-time '20  ;; send first warning 15 minutes before appointment
 appt-display-mode-line nil     ;; don't show in the modeline
 appt-display-format 'window)   ;; pass warnings to the designated window function
(appt-activate 1)                ;; activate appointment notification
(display-time)                   ;; activate time display

;; (org-agenda-to-appt)             ;; generate the appt list from org agenda files on emacs launch
(run-at-time "24:01" 3600 'org-agenda-to-appt)           ;; update appt list hourly
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view

(setq appt-disp-window-function (function my-appt-display))
