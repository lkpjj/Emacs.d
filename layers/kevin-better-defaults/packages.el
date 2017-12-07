;;; packages.el --- kevin-better-defaults layer packages file for Spacemacs.
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

(defconst kevin-better-defaults-packages
  '(
    youdao-dictionary
    git-gutter-fringe
    ))

(defun kevin-better-defaults/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :ensure t
    :config
    (progn
      ;; Enable Cache
      (setq url-automatic-caching t
            ;; Set file path for saving search history
            youdao-dictionary-search-history-file
            (concat spacemacs-cache-directory ".youdao")
            ;; Enable Chinese word segmentation support
            youdao-dictionary-use-chinese-word-segmentation t))))

(defun kevin-better-defaults/init-git-gutter-fringe ()
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
