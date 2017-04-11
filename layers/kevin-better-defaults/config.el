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
(setq default-directory "/Users/liangkai/Code/gopath/src")

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
;; (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

;; dired mode config
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode t)
            (setq dired-omit-files (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|\\.js\\.meta$\\|\\.meta$"))
            ;; always delete and copy recursively
            (setq dired-recursive-deletes 'always)
            (setq dired-recursive-copies 'always)))

;; hook
(add-hook 'prog-mode-hook (lambda ()(goto-address-prog-mode -1)))

;; UI
(setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
      evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
      evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
      evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
      evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
      evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

;; global config
(setq global-hungry-delete-mode t)
