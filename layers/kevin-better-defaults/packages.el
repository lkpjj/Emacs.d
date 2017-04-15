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
  '(youdao-dictionary
    ;; spaceline-all-the-icons
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

;; (defun kevin-better-defaults-packages/init-spaceline-all-theme-icons ()
;;   (use-package spaceline-all-the-icons
;;     :toggle spaceline
;;     :config (spaceline-all-the-icons-theme))
;;   )

;;; packages.el ends here
