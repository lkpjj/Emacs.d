;;; config.el --- custom-ui layer config file for Spacemacs.
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

;; remove menu bar
(setq menu-bar-mode nil)

;; editor config
(setq-default fill-column 80)

;; 标题栏格式设置
(setq frame-title-format
      '("" " Kevin "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))
;; which-func 设置
(which-func-mode)
(defun enable-which-func()
  ;; 从mode line移除which-func
  (setq-default mode-line-misc-info
                (assq-delete-all 'which-func-mode mode-line-misc-info))
  ;; 在header-line 显示which-func
  (setq-default header-line-format
                '((which-func-mode ("" which-func-format " ")))))
;; 只在固定的mode中激活
(add-hook 'prog-mode-hook 'enable-which-func)
(add-hook 'text-mode-hook 'enable-which-func)
(add-hook 'web-mode-hook 'enable-which-func)

;; UI
(setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
      evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
      evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
      evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
      evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
      evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

(setq powerline-default-separator 'arrow)
(spacemacs|diminish hungry-delete-mode)
(spacemacs|diminish git-gutter-mode)
(spacemacs|diminish ggtags-mode)
(spacemacs|diminish spacemacs-whitespace-cleanup-mode)

;; whitespace mode config
(global-whitespace-mode t)
(setq whitespace-style '(face tabs trailing tab-mark))

;; 显示layout
(defun my-update-persp-name ()
  (when (bound-and-true-p persp-mode)
    ;; There are multiple implementations of
    ;; persp-mode with different APIs
    (progn
      (or (not (string= persp-nil-name (safe-persp-name (get-frame-persp))))
          "Default")
      (let ((name (safe-persp-name (get-frame-persp))))
        (propertize (concat "[" name "] ")
                    'face 'font-lock-preprocessor-face
                    'help-echo "Current Layout name.")))))

(setq my-flycheck-mode-line
      '(:eval
        (pcase flycheck-last-status-change
          ((\` not-checked) nil)
          ((\` no-checker) (propertize " -" 'face 'warning))
          ((\` running) (propertize " ✷" 'face 'success))
          ((\` errored) (propertize " !" 'face 'error))
          ((\` finished)
           (let* ((error-counts (flycheck-count-errors flycheck-current-errors))
                  (no-errors (cdr (assq 'error error-counts)))
                  (no-warnings (cdr (assq 'warning error-counts)))
                  (face (cond (no-errors 'error)
                              (no-warnings 'warning)
                              (t 'success))))
             (propertize (format "[%s/%s]" (or no-errors 0) (or no-warnings 0))
                         'face face)))
          ((\` interrupted) " -")
          ((\` suspicious) '(propertize " ?" 'face 'warning)))))


;; 简化 major-mode 的名字，替换表中没有的显示原名
(defun my-simplify-major-mode-name ()
  "Return simplifyed major mode name"
  (let* ((major-name (format-mode-line "%m"))
         (replace-table '(Emacs-Lisp "Elisp"
                                     Spacemacs\ buffer "𝓢"
                                     Python "Py"
                                     ;; Shell ">"
                                     Makrdown "MD"
                                     GFM "𝓜"
                                     Org "lrg"
                                     Text "𝓣ext"
                                     ;; Fundamental "ℱ"
                                     ))
         (replace-name (plist-get replace-table (intern major-name))))
    (if replace-name replace-name major-name
        )))

(defun buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))

;; 自定义mode-line样式
;; anzu 必须加载后才可以设置 anzu--mode-line-format
(with-eval-after-load 'anzu
  (setq-default mode-line-format
                  (list
                  "%1"
                  '(:eval (propertize
                            (window-number-mode-line)
                            'face
                            'font-lock-type-face))
                  " "
                  '(:eval (my-update-persp-name))

                  anzu--mode-line-format

                  "%1"
                  ;; the buffer name; the file name as a tool tip
                  '(:eval (propertize "%b " 'face 'font-lock-keyword-face
                                      'help-echo (buffer-file-name)))


                  "[" ;; insert vs overwrite mode, input-method in a tooltip
                  '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                      'face 'font-lock-preprocessor-face
                                      'help-echo (concat "Buffer is in "
                                                          (if overwrite-mode
                                                              "overwrite"
                                                            "insert") " mode")))

                  ;; was this buffer modified since the last save?
                  '(:eval (when (buffer-modified-p)
                            (concat "," (propertize "Mod"
                                                    'face 'font-lock-warning-face
                                                    'help-echo "Buffer has been modified"))))
                  ;; is this buffer read-only?
                  '(:eval (when buffer-read-only
                            (concat "," (propertize "RO"
                                                    'face 'font-lock-type-face
                                                    'help-echo "Buffer is read-only"))))
                  "]"


                  ;; relative position, size of file
                  " ["
                  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
                  "/"
                  (propertize "%I" 'face 'font-lock-constant-face) ;; size
                  "] "

                  "["
                  ;; the current major mode for the buffer.
                  '(:eval (propertize (my-simplify-major-mode-name) 'face 'font-lock-string-face
                                      'help-echo buffer-file-coding-system))
                  "]"


                  "%1"
                  my-flycheck-mode-line
                  "%1"

                  ;; line and column
                  " (" ;; '%02' to set to 2 chars at least; prevents flickering
                  (propertize "%02l" 'face 'font-lock-type-face) ","
                  (propertize "%02c" 'face 'font-lock-type-face)
                  ") "

                  ;; evil state
                  '(:eval evil-mode-line-tag)


                  ;; git info
                  '(:eval (when (> (window-width) 120)
                            `(vc-mode vc-mode)))

                  ;; minor modes
                  '(:eval (when (> (window-width) 90)
                            minor-mode-alist))

                  " "
                  ;; global-mode-string goes in mode-line-misc-info
                  '(:eval (when (> (window-width) 120)
                            mode-line-misc-info))

                  '(:eval (when (> (window-width) 120)
                            (buffer-encoding-abbrev)))

                  mode-line-end-spaces
                  )))
