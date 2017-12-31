
;;; funcs.el --- kevin-ui-layer layer keybindings file for Spacemacs.
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

;; 显示layout
(defun kevin/update-persp-name ()
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

(setq kevin/flycheck-mode-line
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
(defun kevin/simplify-major-mode-name ()
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

(defun kevin/buffer-encoding-abbrev ()
  "The line ending convention used in the buffer."
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))


(defun mode-line-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to
                                (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun window-number-mode-line ()
  "The current window number. Requires `window-numbering-mode' to be enabled."
  (when (bound-and-true-p window-numbering-mode)
    (let* ((num (window-numbering-get-number))
           (str (when num (int-to-string num))))
      (spaceline--unicode-number str))))

(defun spaceline--unicode-number (str)
  "Return a nice unicode representation of a single-digit number STR."
  (cond
   ((string= "1" str) "➊")
   ((string= "2" str) "➋")
   ((string= "3" str) "➌")
   ((string= "4" str) "➍")
   ((string= "5" str) "➎")
   ((string= "6" str) "➏")
   ((string= "7" str) "➐")
   ((string= "8" str) "➑")
   ((string= "9" str) "➒")
   ((string= "0" str) "➓")))
