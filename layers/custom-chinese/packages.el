;;; packages.el --- custom-chinese layer packages file for Spacemacs.
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

(defconst custom-chinese-packages
  '(
    youdao-dictionary
    pangu-spacing
    pyim
    ))

(defun custom-chinese/init-youdao-dictionary ()
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


(defun custom-chinese/init-pyim ()
  (use-package pyim
    :init
    :config
    ;; 激活 basedict 拼音词库
    (use-package pyim-basedict
      :ensure nil
      :config (pyim-basedict-enable))
    (progn
      (setq pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory))
      (setq pyim-dcache-directory (expand-file-name "dcache/" pyim-directory))
      (setq default-input-method "pyim")
      (setq pyim-default-scheme 'quanpin)
      (setq pyim-page-tooltip t)
      ;; 选词框显示6个候选词
      (setq pyim-page-length 6)
      ;; 让 Emacs 启动时自动加载 pyim 词库
      (add-hook 'emacs-startup-hook
                #'(lambda () (pyim-restart-1 t)))
      ;; 如果当前的 mode 衍生自 prog-mode，那么仅仅在字符串和 comment 中开启中文输入模式
      (setq-default pyim-english-input-switch-functions
                    '(pyim-probe-program-mode))
      (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map))))

(defun custom-chinese/init-pangu-spacing ()
  (use-package pangu-spacing
    :defer t
    :init (progn (global-pangu-spacing-mode 1)
                 (spacemacs|hide-lighter pangu-spacing-mode)
                 ;; Always insert `real' space in org-mode.
                 (add-hook 'org-mode-hook
                           '(lambda ()
                              (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))))
