;;; keybindings.el --- liangkai layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <liangkai@MacBookPro>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)


(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
(define-key global-map (kbd "s-l") 'goto-line)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "<f5>") 'run-current-file)

;;Must set key to nil to prevent error: Key sequence b m s starts with non-prefix key b m
(spacemacs/set-leader-keys "bm" nil)
(spacemacs/set-leader-keys "bD" 'spacemacs/kill-other-buffers)
(spacemacs/declare-prefix "bm" "Bookmark")
(spacemacs/set-leader-keys "bms" 'bookmark-set)
(spacemacs/set-leader-keys "bmr" 'bookmark-rename)
(spacemacs/set-leader-keys "bmd" 'bookmark-delete)
(spacemacs/set-leader-keys "bmj" 'counsel-bookmark)

;; buffer list
(spacemacs/set-leader-keys "bl" 'ibuffer-list-buffers)

;; ivy specific keybindings
(if (configuration-layer/layer-usedp 'ivy)
    (progn
      (spacemacs/set-leader-keys "ff" 'counsel-find-file)
      (spacemacs/set-leader-keys "fL" 'counsel-locate)
      (spacemacs/set-leader-keys "hi" 'counsel-info-lookup-symbol)
      (spacemacs/set-leader-keys "pb" 'projectile-switch-to-buffer)))

;; flycheck error
(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
