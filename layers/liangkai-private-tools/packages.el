;;; packages.el --- liangkai-private-tools layer packages file for Spacemacs.
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

(defconst liangkai-private-tools-packages
  '(
    git-gutter-fringe
    ))

(defun liangkai-private-tools/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :ensure t
    :config
    (progn
      ;; If you enable global minor mode
      (global-git-gutter-mode t)
      (set-face-foreground 'git-gutter-fr:modified "yellow")
      (set-face-foreground 'git-gutter-fr:added    "green")
      (set-face-foreground 'git-gutter-fr:deleted  "red"))))

;;; packages.el ends here
