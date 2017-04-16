;;; keybindings.el --- kevin-better-defaults layer keybindings file for Spacemacs.
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
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"))

(defun goto-match-parent (arg)
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
(defun my-dired-mode-hook ()
  (dired-omit-mode t)
  (setq dired-omit-files (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always))

;; prog mode hook
(defun my-prog-mode-hook ()
  (goto-address-prog-mode nil)
  (spacemacs/toggle-hungry-delete-on))

(defun notify-osx (title message)
  (call-process "terminal-notifier"
                nil 0 nil
                "-group" "Emacs"
                "-title" title
                "-sender" "org.gnu.Emacs"
                "-message" message
                "-activate" "oeg.gnu.Emacs"))

(defun my-appt-display (min-to-app new-time msg)
  (notify-osx
   (format "Appointment in %s minutes" min-to-app)    ;; passed to -title in terminal-notifier call
   (format "%s" msg)))                                ;; passed to -message in terminal-notifier call
