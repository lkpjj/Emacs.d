;;; packages.el --- chinese-layer layer packages file for Spacemacs.
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

(defconst liangkai-chinese-pkgs-packages
  '(
    youdao-dictionary
    pyim
    ))

(defun liangkai-chinese-pkgs/init-youdao-dictionary ()
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


(defun liangkai-chinese-pkgs/init-pyim ()
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
      ;; ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
      ;; ;; 我自己使用的中英文动态切换规则是：
      ;; ;; 1. 光标只有在注释里面时，才可以输入中文。
      ;; ;; 2. 光标前是汉字字符时，才能输入中文。
      ;; ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
      ;; (setq-default pyim-english-input-switch-functions
      ;;               '(pyim-probe-dynamic-english
      ;;                 pyim-probe-isearch-mode
      ;;                 pyim-probe-program-mode
      ;;                 pyim-probe-org-structure-template))
      ;; (setq-default pyim-punctuation-half-width-functions
      ;;               '(pyim-probe-punctuation-line-beginning
      ;;                 pyim-probe-punctuation-after-punctuation))

      ;; 选词框显示5个候选词
      (setq pyim-page-length 6)
      ;; 让 Emacs 启动时自动加载 pyim 词库
      (add-hook 'emacs-startup-hook
                #'(lambda () (pyim-restart-1 t)))
      (evilified-state-evilify pyim-dm-mode pyim-dm-mode-map))))

;;; packages.el ends here
