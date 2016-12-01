;;; packages.el --- liangkai-misc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: liangkai <kevin.scnu@gmail.com>
;; URL: https://github.com/lkpjj/spacemacs.d
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(setq liangkai-misc-packages
      '(evil
        multiple-cursors
        chinese-pyim
        chinese-pyim-greatdict
        youdao-dictionary
        ))

(defun liangkai-misc/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (spacemacs/set-leader-keys "sml" 'mc/edit-lines)
      (spacemacs/set-leader-keys "smb" 'mc/edit-beginnings-of-lines)
      (spacemacs/set-leader-keys "sme" 'mc/edit-ends-of-lines)

      (spacemacs/set-leader-keys "sma" 'mc/mark-all-like-this)
      (spacemacs/set-leader-keys "smA" 'mc/mark-all-dwim)

      (spacemacs/set-leader-keys "smj" 'mc/mark-next-like-this)
      (spacemacs/set-leader-keys "smJ" 'mc/unmark-next-like-this)
      (spacemacs/set-leader-keys "smk" 'mc/mark-previous-like-this)
      (spacemacs/set-leader-keys "smK" 'mc/unmark-previous-like-this)

      (spacemacs/set-leader-keys "smi" 'mc/insert-numbers)
      (spacemacs/set-leader-keys "smh" 'mc-hide-unmatched-lines-mode)
      (spacemacs/set-leader-keys "smd" 'mc/mark-all-symbols-like-this-in-defun)
      (spacemacs/set-leader-keys "smr" 'mc/reverse-regions)
      (spacemacs/set-leader-keys "sms" 'mc/sort-regions)

      (global-unset-key (kbd "M-<down-mouse-1>"))
      (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click))
    :config
    (setq mc/cmds-to-run-for-all
          '(
            electric-newline-and-maybe-indent
            hungry-delete-backward
            spacemacs/backward-kill-word-or-region
            spacemacs/smart-move-beginning-of-line
            lispy-move-beginning-of-line
            lispy-move-end-of-line
            evil-exit-visual-state
            evil-backward-char
            evil-delete-char
            evil-escape-emacs-state
            evil-escape-insert-state
            evil-exit-emacs-state
            evil-previous-visual-line
            evil-next-visual-line
            evil-forward-char
            evil-insert
            evil-next-line
            evil-normal-state
            evil-previous-line
            evil-append
            evil-append-line
            forward-sentence
            kill-sentence
            org-self-insert-command
            sp-backward-delete-char
            sp-delete-char
            sp-remove-active-pair-overlay)))
  )

(defun liangkai-misc/post-init-evil()
  (progn
    (define-key evil-visual-state-map "p" 'evil-paste-after-from-0)
    (define-key evil-insert-state-map (kbd "C-r") 'evil-paste-from-register)

    (define-key evil-normal-state-map (kbd ",a") 'mwim-beginning-of-code-or-line)
    (define-key evil-normal-state-map (kbd ",w") 'evil-write)
    (define-key evil-normal-state-map (kbd ",q") 'evil-quit)
    (define-key evil-normal-state-map (kbd "C-w") 'evil-delete-backward-word)

    (define-key evil-ex-completion-map "\C-a" 'move-beginning-of-line)
    (define-key evil-ex-completion-map "\C-b" 'backward-char)
    (define-key evil-ex-completion-map "\C-k" 'kill-line)

    (define-key evil-visual-state-map (kbd "mn") 'mc/mark-next-like-this)
    (define-key evil-visual-state-map (kbd "mp") 'mc/mark-previous-like-this)
    (define-key evil-visual-state-map (kbd "ma") 'mc/mark-all-like-this)
    (define-key evil-visual-state-map (kbd "mf") 'mc/mark-all-like-this-in-defun)

    (setq evil-normal-state-tag   (propertize "[N]" 'face '((:background "DarkGoldenrod2" :foreground "black")))
          evil-emacs-state-tag    (propertize "[E]" 'face '((:background "SkyBlue2" :foreground "black")))
          evil-insert-state-tag   (propertize "[I]" 'face '((:background "chartreuse3") :foreground "white"))
          evil-motion-state-tag   (propertize "[M]" 'face '((:background "plum3") :foreground "white"))
          evil-visual-state-tag   (propertize "[V]" 'face '((:background "gray" :foreground "black")))
          evil-operator-state-tag (propertize "[O]" 'face '((:background "purple"))))

    ))

(defun liangkai-misc/init-youdao-dictionary ()
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

(defun liangkai-misc/init-chinese-pyim-greatdict ()
    (use-package chinese-pyim-greatdict
    :ensure t
    :config (chinese-pyim-greatdict-enable))
)

(defun liangkai-misc/init-chinese-pyim ()
  (use-package chinese-pyim
    ;; :if (eq 'pinyin chinese-default-input-method)
    :init
    (progn
      (setq pyim-use-tooltip t
            pyim-dicts-directory spacemacs-cache-directory
            pyim-personal-file (concat spacemacs-cache-directory
                                       "pyim-personal.txt")
            default-input-method "chinese-pyim"
            pyim-default-scheme 'quanpin
            )
      (setq pyim-page-length 6)
      (add-hook 'emacs-startup-hook
                #'(lambda () (pyim-restart-1 t)))
      (evilified-state-evilify pyim-dicts-manager-mode pyim-dicts-manager-mode-map))))

;;; packages.el ends here
