;;; packages.el --- liangkai layer packages file for Spacemacs.
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

(defconst liangkai-packages
  '(evil)
)

(defun liangkai/post-init-evil()
  (progn
    (define-key evil-normal-state-map (kbd ",w") 'evil-write)
    (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
    (define-key evil-normal-state-map (kbd ",a") 'mwim-beginning-of-code-or-line)
    ))

;;; packages.el ends here
