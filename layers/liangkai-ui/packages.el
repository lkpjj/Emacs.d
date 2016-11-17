;;; packages.el --- liangkai-ui layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <liangkai@MacBookPro>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst liangkai-ui-packages
  '(highlight-thing)
  )

(defun liangkai-ui/init-highlight-thing()
  (progn
    (global-highlight-thing-mode t)
    (setq highlight-thing-what-thing 'symbol)
    (setq highlight-thing-delay-seconds 0.5)
    (setq highlight-thing-limit-to-defun t)
    (setq highlight-thing-case-sensitive-p t)
    )
  )

;;; packages.el ends here
