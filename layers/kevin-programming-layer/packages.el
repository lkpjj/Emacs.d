;;; packages.el --- kevin-programming-layer layer packages file for Spacemacs.
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

(defconst kevin-programming-layer-packages
  '(
    ;; version control
    git-gutter-fringe
    fringe-helper
    ))

(defun kevin-programming-layer/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :ensure t
    :config
    (progn
      ;; If you enable global minor mode
      (global-git-gutter-mode t)
      (set-face-foreground 'git-gutter-fr:modified "yellow")
      (set-face-foreground 'git-gutter-fr:added    "green")
      (set-face-foreground 'git-gutter-fr:deleted  "red"))))

(defun kevin-programming-layer/init-fringe-helper ()
  (use-package fringe-helper
    :commands (fringe-helper-define fringe-helper-convert)
    :init
    (unless (fboundp 'define-fringe-bitmap)
      ;; doesn't exist in terminal Emacs; define it to prevent errors
      (defun define-fringe-bitmap (&rest _)))
    :after (git-gutter-fringe)
    :config
    (progn
      ;; places the git gutter outside the margins.
      (setq-default fringes-outside-margins t)
      ;; thin fringe bitmaps
      (fringe-helper-define 'git-gutter-fr:added '(center repeated)
        "XXX.....")
      (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
        "XXX.....")
      (fringe-helper-define 'git-gutter-fr:deleted 'bottom
        "X......."
        "XX......"
        "XXX....."
        "XXXX....")
      )))

;;; packages.el ends here
