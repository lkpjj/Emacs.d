;;; config.el --- kevin-programming-layer layer config file for Spacemacs.
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

(add-to-list 'load-path (expand-file-name  "extensions" dotspacemacs-directory))
(add-to-list 'load-path (expand-file-name  "elisp" dotspacemacs-directory))

(add-hook 'prog-mode-hook 'kevin/font-lock-comment-annotations)

(require 'init-auto-save)
