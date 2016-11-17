;;; config.el --- liangkai-ui layer packages file for Spacemacs.
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

;;; Code:
(which-function-mode)
;; when editing js file, this feature is very useful
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" " Liangkai "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name)) "%b"))))

(add-hook 'prog-mode-hook 'linum-mode)

; http://stackoverflow.com/questions/3875213/turning-on-linum-mode-when-in-python-c-mode
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      profiler-report-mode
                                      ffip-diff-mode
                                      dictionary-mode
                                      erc-mode
                                      browse-kill-ring-mode
                                      etags-select-mode
                                      dired-mode
                                      help-mode
                                      text-mode
                                      fundamental-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      inferior-js-mode
                                      inferior-python-mode
                                      inferior-scheme-mode
                                      twittering-mode
                                      compilation-mode
                                      weibo-timeline-mode
                                      woman-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      gnus-group-mode
                                      inf-ruby-mode
                                      gud-mode
                                      org-mode
                                      org-agenda-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      term-mode
                                      spacemacs-buffer-mode
                                      w3m-mode
                                      speedbar-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode
                                      go-mode))

;;; packages.el ends here
