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
;; default directory
(setq-default default-directory "~/Code/gopath/src")

;; org
(defvar org-agenda-dir ""
  "gtd org files location")
(setq-default org-agenda-dir "~/Code/org-notes")
(setq org-agenda-files (list org-agenda-dir))
(setq org-projectile-file (expand-file-name "projects.org" org-agenda-dir))
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-gtd (expand-file-name "gtd.org" org-agenda-dir))
(setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
(setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
(setq org-default-notes-file (expand-file-name "gtd.org" org-agenda-dir))



;; editor config
(setq-default fill-column 80)
;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
(which-function-mode)
;; when editing js file, this feature is very useful
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
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

;; ;; UI
;; (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
;;       evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
;;       evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
;;       evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
;;       evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
;;       evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))
