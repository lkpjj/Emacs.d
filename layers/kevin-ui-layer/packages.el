;;; packages.el --- custom-ui layer packages file for Spacemacs.
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

(defconst kevin-ui-layer-packages
  '(
    (linum :excluded t)
    (linum-relative :excluded t)
    nlinum
    window-numbering
    ))

(defun kevin-ui-layer/init-nlinum ()
  (use-package nlinum
    :init
    :config
    (progn
      (add-hook 'prog-mode-hook 'nlinum-mode)
      ;; (add-hook 'text-mode-hook 'nlinum-mode)
      (setq nlinum-format "%3d"))))


(defun kevin-ui-layer/init-window-numbering ()
  (use-package window-numbering
    :config
    (progn
      (window-numbering-mode)
     )))
