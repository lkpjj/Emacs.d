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
     javascript
     rust
     ivy
     emacs-lisp
     markdown
     org
     osx
     dash
     ranger
     (better-defaults :variables
                      better-defaults-move-to-beginning-of-code-first t
                      better-defaults-move-to-end-of-code-first t)
     (colors :variables
             colors-enable-nyan-cat-progress-bar t)
     (ibuffer	:variables
              ibuffer-group-buffers-by 'projects)
     (go :variables
         go-tab-width 4
         gofmt-command "goimports")
     (git :variables
          git-magit-status-fullscreen t
          magit-push-always-verify nil
          magit-save-repository-buffers 'dontask
          magit-revert-buffers 'silent
          magit-refs-show-commit-count 'all
          magit-revision-show-gravatars nil)
     (shell :variables
            shell-default-shell 'eshell
            shell-default-height 30
            shell-enable-smart-eshell t
            shell-default-position 'bottom)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     (syntax-checking :variables
                      syntax-checking-enable-by-default t
                      syntax-checking-enable-tooltips t)
     (spacemacs-layouts :variables
                        layouts-enable-autosave t
                        layouts-autosave-delay 300)
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      ;; auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      :disabled-for org markdown)
     (gtags :disabled-for clojure emacs-lisp javascript latex python shell-scripts)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     ;; custom layers
     liangkai
     )
   dotspacemacs-additional-packages '()
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(magit-gh-pulls magit-gitflow
                                                   ;; ui
                                                   spaceline volatile-highlights smartparens rainbow-delimiters
                                                   highlight-indentation spacemacs-theme
                                                   ;; evil
                                                   evil-args evil-ediff evil-exchange evil-unimpaired evil-indent-plus evil-mc
                                                   evil-escape evil-lisp-state
                                                   ;; org
                                                   org-bullets org-projectile smooth-scrolling org-repo-todo org-download org-timer
                                                   ;; git
                                                   git-gutter git-gutter-fringe
                                                   ;; mode
                                                   ;; other
                                                   ace-jump-mode ido-vertical-mode clean-aindent-mode
                                                   ac-ispell  smeargle  google-translate gh-md eyebrowse fancy-battery flx-ido disaster lorem-ipsum
                                                   clang-format neotree org-present orgit orglue
                                                   ;; helm
                                                   helm-flyspell flyspell-correct-helm helm-c-yasnippet ace-jump-helm-line helm-make
                                                   helm-themes helm-swoop helm-spacemacs-help helm-dash
                                                   ;; completion
                                                   auto-complete company-quickhelp auto-dictionary
                                                   ;; shell layer
                                                   multi-term term xterm-color vi-tilde-fringe
                                                   spacemacs-purpose-popwin
                                                   ;;ivy-purpose helm-purpose spacemacs-purpose-popwin
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
   dotspacemacs-fullscreen-use-non-native t
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
          ("gnu-cn"   . "https://elpa.zilongshanren.com/gnu/")
          ("melpa"    . "https://melpa.org/packages/")))
  (setq warning-minimum-level :error)
  )

(defun dotspacemacs/user-config ()
  (spacemacs/toggle-truncate-lines-on)
  ;; Visual line navigation for textual modes
  (add-hook 'text-mode-hook 'spacemacs/toggle-visual-line-navigation-on)
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; force horizontal split window
  (setq split-width-threshold 120)
  (linum-relative-on)

  (spacemacs|add-company-hook 'text-mode)

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'text-mode-hook 'spacemacs/toggle-spelling-checking-on)

  (global-hungry-delete-mode t)

  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish ggtags-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish spacemacs-whitespace-cleanup-mode)
  (spacemacs|diminish counsel-mode)

  (evilified-state-evilify-map special-mode-map :mode special-mode)

  (add-to-list 'auto-mode-alist
               '("Capstanfile\\'" . yaml-mode))

  (defun toggle-major-mode ()
    (interactive)
    (if (eq major-mode 'fundamental-mode)
        (set-auto-mode)
      (fundamental-mode)))
  (spacemacs/set-leader-keys "otm" 'toggle-major-mode)


  (defun spacemacs/ivy-persp-switch-project (arg)
    (interactive "P")
    (ivy-read "Switch to Project Perspective: "
              (if (projectile-project-p)
                  (cons (abbreviate-file-name (projectile-project-root))
                        (projectile-relevant-known-projects))
                projectile-known-projects)
              :action (lambda (project)
                        (let ((persp-reset-windows-on-nil-window-conf t))
                          (persp-switch project)
                          (let ((projectile-completion-system 'ivy)
                                (old-default-directory default-directory))
                            (projectile-switch-project-by-name project)
                            (setq default-directory old-default-directory))))))

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
  )

(setq custom-file (expand-file-name "custom.el" dotspacemacs-directory))
(load custom-file 'no-error 'no-message)
