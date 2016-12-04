;;; config.el --- liangkai layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <kevin.scnu@gmail.com>
;; URL: https://github.com/lkpjj/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

(when (spacemacs/window-system-is-mac)
     (setq ns-pop-up-frames nil))

(setq default-directory "/Users/liangkai/Code/gopath/src")

;; remove ^ character when use M-x
(setq ivy-initial-inputs-alist nil)
;; fullframe when use magit
(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq gc-cons-threshold 100000000)

(global-prettify-symbols-mode 1)

(setq-default fill-column 80)
(setq ns-pop-up-frames nil)
(setq user-mail-address "kevin.scnu@gmail.com")
(setq user-full-name "liangkai")

(setq recenter-positions '(top middle bottom))
;; delete the selection with a key press
(delete-selection-mode t)

;;add auto format paste code
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode
                     scheme-mode
                     haskell-mode
                     ruby-mode
                     rspec-mode
                     python-mode
                     c-mode
                     c++-mode
                     objc-mode
                     latex-mode
                     js-mode
                     plain-tex-mode
                     go-mode))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
(remove-hook 'find-file-hooks 'vc-find-file-hook)

(setq large-file-warning-threshold 100000000)

(setq save-abbrevs nil)

;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(setq url-show-status nil)

;;Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; cleanup recent files
(defun cleanup-recentf-and-known-projects ()
  (progn
    (and (fboundp 'recentf-cleanup)
         (recentf-cleanup))
    (and (fboundp 'projectile-cleanup-known-projects)
         (projectile-cleanup-known-projects))))
(add-hook 'kill-emacs-hook #'cleanup-recentf-and-known-projects)

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(show-paren-mode t)

;;; config.el ends here
