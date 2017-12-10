;;; keybindings.el --- custom-layers layer keybindings file for Spacemacs.
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

;; A complementary binding to the apropos-command (C-h a)
(define-key 'help-command "A" 'apropos)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-i") 'info-display-manual)


;; evil keybindings
(define-key evil-normal-state-map (kbd ",a") 'mwim-beginning-of-code-or-line)
(define-key evil-normal-state-map (kbd ",w") 'evil-write)
(define-key evil-normal-state-map (kbd ",W") 'evil-write-all)
(define-key evil-normal-state-map (kbd ",q") 'evil-quit)
(define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)
(define-key evil-motion-state-map (kbd "C-i") 'evil-jump-forward)
(define-key evil-motion-state-map (kbd "C-o") 'evil-jump-backward)

(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)

;; cursor move
(define-key evil-insert-state-map (kbd "C-a") 'mwim-beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-motion-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-normal-state-map (kbd "C-e") 'mwim-end-of-code-or-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(spacemacs/set-leader-keys "jp" 'goto-match-parent)
;; (spacemacs/set-leader-keys "jl" 'goto-line)

(global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
(global-set-key (kbd "<f5>") 'run-current-file)

;; 自定义按键
;; buffer 相关
(spacemacs/set-leader-keys "bm" 'spacemacs/kill-other-buffers)
(spacemacs/set-leader-keys "bl" 'ibuffer-list-buffers)
(spacemacs/set-leader-keys "bg" 'revert-buffer)

;; helm
(global-set-key (kbd "C-s") 'helm-swoop)

;; 自定义快捷键
;; bookmark
(spacemacs/set-leader-keys "ob" nil)
(spacemacs/declare-prefix "ob" "bookmark")
(spacemacs/set-leader-keys "obs" 'bookmark-set)
(spacemacs/set-leader-keys "obr" 'bookmark-rename)
(spacemacs/set-leader-keys "obd" 'bookmark-delete)
(spacemacs/set-leader-keys "obj" 'helm-filtered-bookmarks)
(spacemacs/set-leader-keys "obl" 'bookmark-bmenu-list)
;; toggle
(spacemacs/set-leader-keys "ot" nil)
(spacemacs/declare-prefix "ot" "toggle")
(spacemacs/set-leader-keys "otm" 'toggle-major-mode)
(spacemacs/set-leader-keys "otb" 'toggle-scroll-bar)
(spacemacs/set-leader-keys "otw" 'toggle-word-wrap)

(spacemacs/set-leader-keys "oi" 'iterm-focus)

;; flycheck error
(spacemacs/set-leader-keys "en" 'flycheck-next-error)
(spacemacs/set-leader-keys "ep" 'flycheck-previous-error)
