;;; packages.el --- liangkai-ui layer packages file for Spacemacs.
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

;;; Code:

(defconst liangkai-ui-packages
  '(highlight-thing
    vline)
  )

(defun liangkai-ui/init-highlight-thing()
  (progn
    ;; (global-highlight-thing-mode t)
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun t)
    (setq highlight-thing-case-sensitive-p t)
    )
  )

(defun liangkai-ui/init-vline ()
  "高亮当前列"
  (use-package vline
    :defer t
    :init
    (spacemacs/set-leader-keys
      "otc" #'vline-mode
      "otC" #'vline-global-mode)
    :config
    ;; 与默认的行高亮的颜色相同
    (set-face-background vline-face "#073642")))

;;; packages.el ends here
