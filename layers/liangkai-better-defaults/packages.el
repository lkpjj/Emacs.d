;;; packages.el --- liangkai-better-defaults layer packages file for Spacemacs.
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

(defconst liangkai-better-defaults-packages
  '(
    (dired-mode :location built-in)
    ;; (profiler :location built-in)
    (recentf :location built-in)
    )
  )

(defun liangkai-better-defaults/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"))
    (setq recentf-max-saved-items 2048)))

  (defun liangkai-better-defaults/init-dired-mode ()
  (use-package dired-mode
    :defer t
    :init
    (progn
      (require 'dired-x)
      (require 'dired-aux)
      (setq dired-listing-switches "-alh")
      (setq dired-guess-shell-alist-user
            '(("\\.pdf\\'" "open")
              ("\\.docx\\'" "open")
              ("\\.\\(?:djvu\\|eps\\)\\'" "open")
              ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" "open")
              ("\\.\\(?:xcf\\)\\'" "open")
              ("\\.csv\\'" "open")
              ("\\.tex\\'" "open")
              ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\(?:\\.part\\)?\\'"
               "open")
              ("\\.\\(?:mp3\\|flac\\)\\'" "open")
              ("\\.html?\\'" "open")
              ("\\.md\\'" "open")))

      (setq dired-omit-files
      (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$"))

      ;; always delete and copy recursively
      (setq dired-recursive-deletes 'always)
      (setq dired-recursive-copies 'always)

      (defvar dired-filelist-cmd
        '(("vlc" "-L")))

      ;; ;; FIXME: evilify dired mode will lead to startup warnings
      ;; (evilified-state-evilify-map dired-mode-map
      ;;   :mode dired-mode
      ;;   :bindings
      ;;   (kbd "C-k") 'dired-up-directory
      ;;   "<RET>" 'dired-find-alternate-file
      ;;   "E" 'dired-toggle-read-only
      ;;   "C" 'dired-do-copy
      ;;   "<mouse-2>" 'my-dired-find-file
      ;;   "`" 'dired-open-term
      ;;   "p" 'peep-dired-prev-file
      ;;   "n" 'peep-dired-next-file
      ;;   "z" 'dired-get-size
      ;;   "c" 'dired-copy-file-here
      ;;   ")" 'dired-omit-mode)
      )
    ))

;;; packages.el ends here
