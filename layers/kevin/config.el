;;; config.el --- kevin layer config file f Spacemacs.
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


;; org files directory
(defvar org-agenda-dir "~/Code/org-notes"
  "gtd org files location")
(setq-default org-agenda-dir "~/Code/org-notes")

;; default directory
(setq-default default-directory "~/Code/gopath/src")

;; helm config
;; move helm input in minibuffer
(setq helm-echo-input-in-header-line nil)
(remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;; hook
(add-hook 'dired-mode-hook 'kevin/dired-mode-hook)
(add-hook 'prog-mode-hook 'kevin/prog-mode-hook)
(add-hook 'after-save-hook 'kevin/revert-buffer-no-confirm)
;; which func mode
(add-hook 'prog-mode-hook 'enable-which-func)
(add-hook 'text-mode-hook 'enable-which-func)
(add-hook 'web-mode-hook 'enable-which-func)
