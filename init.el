;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  (setq-default
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     rust
     (go :variables go-tab-width 4
         gofmt-command "goimports")
     ivy
     better-defaults
     emacs-lisp
     git
     markdown
     org
     dash
     ranger
     (osx :variables osx-dictionary-dictionary-choice "Simplified Chinese - English")
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-default-position 'bottom)
     (spell-checking :variables spell-checking-enable-by-default nil)
     (syntax-checking :variables syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     ;; version-control
     colors
     (auto-completion :variables auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org markdown)
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (python :variables
             python-test-runner '(nose pytest))
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     (chinese :packages youdao-dictionary fcitx
              :variables chinese-enable-fcitx nil
              chinese-enable-youdao-dict t
              )
     liangkai
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(magit-gh-pulls magit-gitflow org-projectile evil-mc
                                                   evil-args evil-ediff evil-exchange evil-unimpaired
                                                   evil-indent-plus volatile-highlights smartparens
                                                   spaceline holy-mode skewer-mode rainbow-delimiters
                                                   highlight-indentation vi-tilde-fringe eyebrowse
                                                   org-bullets smooth-scrolling org-repo-todo org-download org-timer
                                                   livid-mode git-gutter git-gutter-fringe  evil-escape
                                                   leuven-theme gh-md evil-lisp-state spray lorem-ipsum
                                                   ac-ispell ace-jump-mode auto-dictionary
                                                   clang-format define-word google-translate disaster epic
                                                   fancy-battery neotree org-present orgit orglue spacemacs-theme
                                                   helm-flyspell flyspell-correct-helm clean-aindent-mode
                                                   helm-c-yasnippet ace-jump-helm-line helm-make
                                                   helm-themes helm-swoop helm-spacemacs-help smeargle
                                                   ido-vertical-mode flx-ido company-quickhelp
                                                   auto-complete
                                                   )
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update t
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(solarized-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Monaco"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text t
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup t
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server t
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup `trailing
   ))

(defun dotspacemacs/user-init ()
  (setq configuration-layer--elpa-archives
        '(("melpa-cn" . "https://elpa.zilongshanren.com/melpa/")
          ("org-cn"   . "https://elpa.zilongshanren.com/org/")
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")))
  (setq warning-minimum-level :error)
  )

(defun dotspacemacs/user-config ()

  (global-set-key (kbd "<backtab>") 'un-indent-by-removing-4-spaces)
  (defun un-indent-by-removing-4-spaces ()
    "remove 4 spaces from beginning of of line"
    (interactive)
    (save-excursion
      (save-match-data
        (beginning-of-line)
        ;; get rid of tabs at beginning of line
        (when (looking-at "^\\s-+")
          (untabify (match-beginning 0) (match-end 0)))
        (when (looking-at (concat "^" (make-string tab-width ?\ )))
          (replace-match "")))))

  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)

  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
