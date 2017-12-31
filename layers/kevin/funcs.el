;;; funcs.el --- kevin layer keybindings file for Spacemacs.
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
(defun kevin/iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun kevin/goto-match-parent (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
  (cond ((looking-at "[\[\(\{]") (evil-jump-item))
        ((looking-back "[\]\)\}]" 1) (evil-jump-item))
        ;; now, try to succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
        ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
        (t nil)))

;; dired mode config
(defun kevin/dired-mode-hook ()
  (dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always))

;; prog mode hook
(defun kevin/prog-mode-hook ()
  (goto-address-prog-mode nil)
  (spacemacs/toggle-hungry-delete-on))

(defun enable-which-func()
  ;; 从mode line移除which-func
  (setq-default mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info))
  ;; 在header-line 显示which-func
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " ")))))


(defun kevin/terminal-notification (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message
                "-activate" "oeg.gnu.Emacs"))

(defun kevin/appt-notification (min-to-app new-time msg)
  (kevin/terminal-notification
   (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "%s" msg)))                                ;; passed to -message in terminal-notifier call

(defun kevin/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

;; http://blog.lojic.com/2009/08/06/send-growl-notifications-from-carbon-emacs-on-osx/
(defun kevin/growl-notification (title message &optional sticky)
  "Send a Growl notification"
  (interactive)
  (do-applescript
   (format "tell application \"GrowlHelperApp\" \n
              notify with name \"Emacs Notification\" title \"%s\" description \"%s\" application name \"Emacs.app\" sticky \"%s\"
              end tell
              "
           title
           message
           (if sticky "yes" "no"))))

(defun kevin/growl-timer (minutes message)
  "Issue a Growl notification after specified minutes"
  (interactive (list (read-from-minibuffer "Minutes: " "10")
                     (read-from-minibuffer "Message: " "Reminder") ))
  (run-at-time (* (string-to-number minutes) 60)
               nil
               (lambda (minute message)
                 (kevin/growl-notification "Emacs Reminder" message t))
               minutes
               message))

(defun kevin/growl-test ()
  (interactive)
  ;; (kevin/growl-notification "Emacs Notification" "This is my message.")
  (kevin/growl-notification "Emacs Notification" "This is my sticky message." t))

;; 快速打开org mode gtd file.
(defun kevin/open-org-gtd-file()
  "Open org mode gtd.org file"
  (interactive)
  (find-file (expand-file-name org-agenda-dir "gtd.org")))

;; 高亮TODO、NOTE etc.
(defun kevin/font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil
   '(("\\<\\(FIX\\(ME\\)?\\|BUG\\|HACK\\):" 1 font-lock-warning-face t)
     ("\\<\\(NOTE\\):" 1 'org-level-2 t)
     ("\\<\\(TODO\\):" 1 'org-todo t)
     ("\\<\\(DONE\\):" 1 'org-done t))
   ))
