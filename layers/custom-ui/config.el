;;; config.el --- custom-ui layer config file for Spacemacs.
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

;; remove menu bar
(setq menu-bar-mode nil)

;; editor config
(setq-default fill-column 80)

;; 标题栏格式设置
(setq frame-title-format
      '("" " Kevin "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
;; which-func 设置
(which-func-mode)
(defun enable-which-func()
  ;; 从mode line移除which-func
  (setq-default mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info))
  ;; 在header-line 显示which-func
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " ")))))
;; 只在固定的mode中激活
(add-hook 'prog-mode-hook 'enable-which-func)
(add-hook 'text-mode-hook 'enable-which-func)
(add-hook 'web-mode-hook 'enable-which-func)

;; UI
(setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
      evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
      evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
      evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
      evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
      evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

(setq powerline-default-separator 'arrow)
(spacemacs|diminish hungry-delete-mode)
(spacemacs|diminish git-gutter-mode)
(spacemacs|diminish ggtags-mode)
(spacemacs|diminish spacemacs-whitespace-cleanup-mode)

;; whitespace mode config
(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing tab-mark))
