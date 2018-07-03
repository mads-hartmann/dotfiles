;;; init.el --- Mads' configuration file
;;; Commentary:
;;; Code:

;; Package Manager
;; ---------------------------------
(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("melpa" . "http://melpa.milkbox.net/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Global Variables
;; ---------------------------------
(setq
 inhibit-startup-screen t
 create-lockfiles nil
 make-backup-files nil
 auto-save-default nil
 column-number-mode t
 scroll-error-top-bottom t
 show-paren-delay 0.1
 use-package-verbose nil
 use-package-always-ensure t
 package-enable-at-startup nil
 sentence-end-double-space nil
 split-width-threshold nil
 split-height-threshold nil
 mac-allow-anti-aliasing t
 ring-bell-function 'ignore
 inhibit-startup-echo-area-message t
 inhibit-startup-message t
 frame-title-format '((:eval buffer-file-name))
 enable-local-variables :all
 mouse-1-click-follows-link t
 mouse-1-click-in-non-selected-windows t
 select-enable-clipboard t
 mouse-wheel-scroll-amount '(0.01)
 column-number-mode t
 confirm-kill-emacs (quote y-or-n-p)
 ns-use-native-fullscreen nil
 ns-pop-up-frames nil
 mac-option-modifier 'meta
 mac-command-modifier 'super
 debug-on-error nil
 line-move-visual t
 custom-file "~/.emacs.d/custom.el"
 explicit-shell-file-name "/bin/bash"
 shell-file-name "bash"
 imenu-auto-rescan t)

;; Buffer Local Variables
;; ---------------------------------
(setq-default
 fill-column 70
 indent-tabs-mode nil
 truncate-lines t
 require-final-newline t
 indicate-empty-lines t
 fringe-mode '(4 . 2))

;; Aliasess
;; ---------------------------------
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable Protected Features
;; ---------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Modes (that ships with Emacs)
;; ---------------------------------

(pending-delete-mode t)
(normal-erase-is-backspace-mode t)
(delete-selection-mode 1)
(show-paren-mode t)
(global-auto-revert-mode t)
(electric-pair-mode 1)
(electric-indent-mode -1)
(global-hl-line-mode -1)
(global-hi-lock-mode -1)

;; Environment Variables
;; ---------------------------------
;; I prefer to set up my env manually here. I don't use the shell often
;; from within Emacs so it's no so important for me that it inherits my
;; normal shell config.
(setenv "SHELL" shell-file-name)
(setenv
 "PATH"
 (mapconcat
  'identity
  '("/usr/local/sbin"
    "/usr/local/bin"
    "/usr/bin"
    "/bin"
    "/usr/sbin"
    "/sbin")
  ":"))

;; global Keybindings
;; ---------------------------------
(global-set-key (kbd "s-a") 'mark-whole-buffer)
(global-set-key (kbd "s-v") 'yank)
(global-set-key (kbd "s-c") 'kill-ring-save)
(global-set-key (kbd "s-z") 'undo)
(global-set-key (kbd "s-x") 'kill-region)
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-l") 'goto-line)
(global-set-key (kbd "s-w") 'delete-frame)
(global-set-key (kbd "s-n") 'new-frame)
(global-set-key (kbd "s-d") 'duplicate-line)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-`") 'ns-next-frame)
(global-set-key (kbd "s-¬") 'ns-prev-frame)
(global-set-key (kbd "<backtab>") nil)  ; Wanted it to de-indent, but that's not really a thing
(global-set-key [(super shift return)] 'toggle-maximize-buffer)
(global-set-key (kbd "C-o") 'open-line)
(global-set-key (kbd "M-.") 'mhj/find-tag)
(global-set-key (kbd "s-.") 'mhj/tags-apropos)
(global-set-key (kbd "<s-mouse-1>") 'xref-find-definitions)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "s-/") 'comment-line-dwim)
(global-set-key (kbd "C-a") 'beginning-of-line-or-indentation)
(global-set-key (kbd "s-<return>") 'toggle-fullscreen)
(global-set-key (kbd "C-x C-SPC") 'pop-to-mark-command)
(global-set-key (kbd "s-[") 'prev-window)
(global-set-key (kbd "s-]") 'other-window)
(global-set-key (kbd "M-a") 'insert-aa) ; For when I want to
(global-set-key (kbd "M-o") 'insert-oe) ; write danish with my
(global-set-key (kbd "M-'") 'insert-ae) ; uk layout keyboard.
(global-set-key (kbd "C-c C-1") 'previous-buffer)
(global-set-key (kbd "C-c C-2") 'next-buffer)
(global-set-key (kbd "C-<up>") (lambda () (interactive) (scroll-up -1)))
(global-set-key (kbd "C-<down>") (lambda () (interactive) (scroll-down -1)))
(global-set-key (kbd "<f1>") (lambda () (interactive) (switch-to-buffer nil)))
(global-set-key (kbd "<f3>") 'highlight-symbol)
(global-set-key (kbd "<f4>") 'pop-tag-mark)
(global-set-key (kbd "<f5>") 'xref-find-definitions)
(global-set-key (kbd "<f11>") 'mhj/show-info-sidebar)
;; Using neo-tree for a bit. So won't need this for the duration.
(global-set-key (kbd "<f12>") 'mhj/toggle-project-explorer)
(global-set-key (kbd "s-0") 'mhj/focus-project-explorer)
(define-key isearch-mode-map (kbd "<backspace>") 'isearch-delete-char)

;; Load my various elisp files.
;; ---------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load "~/.emacs.d/functions.el")
(load custom-file 'noerror)

;; window-system specific configuration
;; ---------------------------------
(if window-system
    (progn
      (setq server-name "server-gui")
      (server-start)
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (add-to-list 'initial-frame-alist '(width . 150))
      (add-to-list 'initial-frame-alist '(height . 50))
      (add-to-list 'default-frame-alist '(width . 150))
      (add-to-list 'default-frame-alist '(height . 50))
      (load-theme 'base16-ocean-dark-hartmann t)
      (set-face-attribute 'default nil :font "Fira Code-14:antialias=subpixel"))
  (progn
    ;; Defines to interact with the osx clipbord.
    ;; This is only relevant when I'm using Emacs from the terminal (right now)
    (setq interprogram-cut-function 'paste-to-osx
          interprogram-paste-function 'copy-from-osx)

    (menu-bar-mode -1)
    (xterm-mouse-mode t)
    (global-set-key (kbd "C-M-d") 'backward-kill-word)))

;; Configuration of modes
;; ---------------------------------

(use-package conf-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))))

(use-package compile
  ;; Configuration of the built-in compilation-mode
  :ensure nil
  :config
  (progn
    (setq compilation-scroll-output t)
    (setq compilation-read-command nil)
    (setq compilation-ask-about-save nil) ; Automatically save buffers before compiling
    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*compilation*" eos)
                   (display-buffer-reuse-window
                    display-buffer-in-side-window)
                   (reusable-frames . visible)
                   (side            . bottom)
                   (window-height   . 0.3)))))

(use-package dabbrev
  ;; configuration of the built-in dynamic abbreviation package.
  :bind ("M-/" . dabbrev-expand)
  :init
  (progn
    (setq dabbrev-case-replace nil)
    (setq dabbrev-case-distinction nil)
    (setq dabbrev-case-fold-search nil)))

(use-package linum
  ;; Configuration of the built-in linum-mode.
  :config
  (progn
    (global-linum-mode -1)
    (setq linum-format "%4d ")))

(use-package sh-script
  ;; Configuration of the built-in sh-mode
  :config
  (progn
    (add-hook 'sh-mode-hook 'flycheck-mode)
    (add-hook 'sh-mode-hook 'linum-mode)
    (add-hook 'sh-mode-hook 'company-mode)))

(use-package make-mode
  ;; Configuration of the built-in makefile-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\Makefile\\'" . makefile-mode))
    (add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))

    (font-lock-add-keywords
     'makefile-mode
     '(("define" . font-lock-keyword-face)
       ("endef" . font-lock-keyword-face)
       ("ifeq" . font-lock-keyword-face)
       ("ifneq" . font-lock-keyword-face)
       ("ifdef" . font-lock-keyword-face)
       ("ifndef" . font-lock-keyword-face)
       ("else" . font-lock-keyword-face)
       ("endif" . font-lock-keyword-face)))

    (defun makefile-mode-setup ()
      (setq whitespace-style '(face tab-mark trailing)))

    (add-hook 'makefile-mode-hook 'linum-mode)
    (add-hook 'makefile-mode-hook 'makefile-mode-setup)))

(use-package macrostep
  ;; Awesome little package for expanding macros. Helps to understand
  ;; what is going on im my use-package declarations.
  :bind ("C-c e m" . macrostep-expand))

(use-package ibuffer-projectile)
(use-package ibuffer
  ;; A different buffer view.
  :bind ("C-x C-b" . ibuffer)
  :init
  (progn
    (require 'ibuf-ext)

    (setq ibuffer-filter-group-name-face 'success) ; TODO: declare it's own face.
    (setq ibuffer-show-empty-filter-groups nil)
    (add-to-list 'ibuffer-never-show-predicates "^\\*")

    (add-hook 'ibuffer-mode-hooks 'ibuffer-auto-mode)

    (setq ibuffer-formats
          '((mark modified read-only " " (name 18 18 :left :elide) " " (size 9 -1 :right) " " (mode 16 16 :left :elide) " " filename-and-process)
            (mark " " (name 16 -1) " " filename)))))


(use-package ace-jump-mode
  ;; Quick way to jump to a given char.
  :bind ("C-<tab>" . ace-jump-mode))

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
        ("<s-down>" . dired-find-file)
        ("<s-up>" . diredp-up-directory))
  :init
  (progn
    (require 'dired-x)
    (setq
     insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"
     dired-listing-switches "-lAXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'hl-line-mode)
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(use-package dired-narrow
  ;; Make it possible to filter/search in a dired buffer. After a
  ;; filter has been applied it can be removed by refreshing the
  ;; buffer with 'g'.
  :bind
  (:map dired-mode-map
        ("/" . dired-narrow)))

(use-package dired-subtree
  ;; Very helpful package that makes it possible to insert a dired
  ;; subtree buffer directly below a folder in a dired buffer. Give
  ;; you something similar to a tree explorer.
  :demand
  :bind
  (:map dired-mode-map
        ("<enter>" . mhj/dwim-toggle-or-open)
        ("<return>" . mhj/dwim-toggle-or-open)
        ("<tab>" . mhj/dwim-toggle-or-open)
        ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

(use-package exec-path-from-shell
  ;; Make sure that emacs inherit environment variables from ~/.zshenv
  ;; and files like that.
  :init
  (progn
    (setq exec-path-from-shell-check-startup-files nil)
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "GOPATH")))

(use-package sh-script
  :config
  (progn
    (add-hook 'sh-mode-hook 'linum-mode)))

(use-package shell
  :commands shell
  :bind
  (:map shell-mode-map
        ("s-k" . clear-shell)
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input)))

(use-package flyspell
  ;; Spell-checking of emacs buffers.
  :diminish (flyspell-mode)
  :commands flyspell-mode
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil)
        ("C-," . nil)
        ("C-." . nil))
  :init
  (progn
    (add-hook 'git-commit-mode-hook 'flyspell-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))

(use-package ido
  ;; interactively-do-things. Improved find-file and M-x.
  :init
  (progn
    (defun ido-M-x ()
      (interactive)
      (call-interactively
       (intern
        (ido-completing-read
         "M-x "
         (all-completions "" obarray 'commandp)))))

    (ido-mode 1)
    (setq ido-enable-flex-matching t)
    (setq ido-use-filename-at-point nil)
    (setq ido-create-new-buffer 'always)
    (setq ido-max-prospects 20)
    (setq ido-auto-merge-work-directories-length -1))) ; disable annoying directory search

(use-package ido-vertical-mode
  ;; Display suggestions in the ido minibuffer vertically.
  :init
  (progn
    (ido-vertical-mode 1)
    (defun bind-ido-keys ()
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p")   'ido-prev-match))
    (add-hook 'ido-setup-hook 'bind-ido-keys)))

(use-package magit
  :demand
  :commands magit-status
  :config
  (progn
    (setq magit-last-seen-setup-instructions "1.4.0")
    (setq magit-push-always-verify `PP)))

(use-package projectile
  :diminish ""
  :bind
  (:map projectile-mode-map
        ("C-c p f" . projectile-find-file)
        ("C-c p p" . projectile-switch-project))
  :init
  (progn
    (projectile-global-mode)
    (setq projectile-switch-project-action 'helm-projectile-find-file)
    (setq projectile-completion-system 'helm) ;; alternatively, 'ido
    (setq projectile-use-git-grep t)))

(use-package helm
  :bind (("C-." . helm-M-x)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-s" . helm-occur))
  :init
  (progn
    (setq helm-follow-mode t)
    (setq helm-full-frame nil)
    (setq helm-split-window-inside-p t)
    (setq helm-split-window-default-side 'below)
    (setq helm-buffer-max-length nil)

    (setq helm-buffers-fuzzy-matching t)
    (setq helm-M-x-always-save-history nil)

    (setq helm-find-files-actions '
          (("Find File" . helm-find-file-or-marked)
           ("View file" . view-file)
           ("Zgrep File(s)" . helm-ff-zgrep)))

    (setq helm-type-file-actions
          '(("Find File" . helm-find-file-or-marked)
            ("View file" . view-file)
            ("Zgrep File(s)" . helm-ff-zgrep)))

    (add-to-list 'display-buffer-alist
                 `(,(rx bos "*helm" (+ anything) "*" eos)
                   (display-buffer-in-side-window)
                   (side            . bottom)
                   (window-height   . 0.3)))))

(use-package helm-c-yasnippet
  :demand
  :bind ("C-c y" . helm-yas-complete))

(use-package helm-projectile
  :bind (("s-t" . helm-projectile-find-file)
         ("C-," . helm-projectile))
  :config
  (progn
    ;; Removes 'helm-source-projectile-projects' from C-c p h as it is
    ;; possible to switch project using 'helm-projectile-switch-project'

    ;; other options:
    ;;    helm-source-projectile-files-list
    ;;    helm-source-projectile-recentf-list
    (setq helm-projectile-sources-list
          '(helm-source-projectile-buffers-list))))

(use-package helm-git-grep
  ;; Interactive git-grep using helm
  :bind (("s-F" . helm-git-grep)))

(use-package helm-ag
  ;; Interactive ag queries using helm.
  :disabled
  :bind (("s-F" . helm-projectile-ag)))

(use-package helm-ls-git
  ;; Pretty nice project overview
  :config
  (progn
    (setq helm-ls-git-default-sources
          '(helm-source-ls-git-buffers
            helm-source-ls-git-status
            helm-source-ls-git))
    (setq helm-ls-git-show-abs-or-relative 'relative)))

(use-package expand-region
  ;; One of my favorite packages. Can increase/shrink a selection in
  ;; clever ways.
  :bind ("C-w" . er/expand-region))

(use-package auto-complete
  ;; Code-completion backend.
  ;; TODO: Would prefer to use company mode everywhere.
  :diminish (auto-complete-mode)
  :init
  (progn (global-auto-complete-mode t))
  :bind
  (:map ac-complete-mode-map
        ("\C-n" . ac-next)
        ("\C-p" . ac-previous))
  :config (progn
            (ac-config-default)
            (setq ac-auto-start nil)
            (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")))

(use-package multiple-cursors
  :bind (("C-M->" . mc/unmark-next-like-this)
         ("C-M-<" . mc/unmark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package ag
  :commands ag
  :config
  (progn
    (setq ag-highlight-search t)
    (setq ag-reuse-buffers 't)))

(use-package undo-tree
  :diminish (undo-tree-mode)
  :bind (("C-x u" . undo-tree-visualize))
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-relative-timestamps t)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff nil)

    (add-to-list
     'display-buffer-alist
     `(,(rx bos " *undo-tree*" eos)
       (display-buffer-in-side-window)
       (side . bottom)
       (window-height . 0.4)))))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :defer
  :init
  (progn
    (yas-global-mode 1)
    (setq
     yas-indent-line 'auto
     yas-also-auto-indent-first-line t
     yas-also-indent-empty-lines t)
    (setq yas-snippet-dirs '("~/.snippets"))
    (yas-reload-all)))

(use-package diff-hl
  :bind
  (:map diff-hl-mode-map
        ("M-g l" . diff-hl-diff-goto-hunk))
  :init (global-diff-hl-mode))

(use-package org
  :bind
  (:map org-mode-map
        ("C-," . nil)
        ("C-c C-j" . helm-org-in-buffer-headings))
  :config
  (progn
    (define-key 'org-mode-map (kbd "C-<tab>") nil)
    (add-hook 'org-mode-hook 'linum-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))))

(use-package flycheck-gometalinter
  :config
  (progn
    (flycheck-gometalinter-setup)))

(use-package go-autocomplete)
(use-package go-mode
  :bind (("M-." . godef-jump)
         ("M-," . pop-tag-mark)
         ("C-c C-c" . compile)
         ("M-<tab>" . ac-complete-go))
  :config
  (progn
    (require 'go-autocomplete)
    (require 'auto-complete-config)
    (ac-config-default)

    ; Use goimports instead of go-fmt
    (setq gofmt-command "goimports")

    (add-hook 'go-mode-hook
              (lambda ()
                ;; Go uses tabs :O
                ;; Deactivate my otherwise agressive styling of tabs
                ;; for any buffer that using go-mode.
                (setq-local whitespace-style '(trailing face))
                ;; Seems to handle auto-indentation poorly.
                (setq-local yas-indent-line 'fixed)))

    ; Customize compile command to run go build
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go vet"))

    ;; go get github.com/rogpeppe/godef
    ;; go get -u github.com/nsf/gocode
    ;; go get golang.org/x/tools/cmd/goimports
    ;; go get golang.org/x/tools/cmd/guru
    ;; go get -v github.com/ramya-rao-a/go-outline
    ;; go get -v github.com/uudashr/gopkgs/cmd/gopkgs
    ;; go get -v sourcegraph.com/sqs/goreturns
    (add-hook 'before-save-hook 'gofmt-before-save)))

(use-package css-mode
  :bind
  (:map css-mode-map
        ("M-<tab>" . company-complete))
  :config
  (progn
    (add-hook 'css-mode-hook 'linum-mode)
    (add-hook 'css-mode-hook 'company-mode)))

(use-package scss-mode
  :commands scss-mode
  :config
  (progn
    (setq scss-compile-at-save nil)
    (add-hook 'scss-mode-hook 'linum-mode)))

(use-package imenu-list
  :ensure t
  :bind (("C-c o" . imenu-list-smart-toggle))
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize nil))

(use-package markdown-mode
  :commands markdown-mode
  :bind
  (:map markdown-mode-map
        ("M-<tab>" . ido-complete-word-ispell)
        ("M-?" . ispell-word)
        ("C-c C-c" . flycheck-list-errors))
  :config
  (progn
    (custom-set-variables
     '(markdown-hide-markup t)
     '(markdown-fontify-code-blocks-natively t))
    (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
    (add-hook 'markdown-mode-hook 'linum-mode)
    (add-hook 'markdown-mode-hook 'flycheck-mode)
    (add-hook 'markdown-mode-hook 'flyspell-mode)))

(use-package eldoc
  :diminish (eldoc-mode))

(use-package paredit)

(use-package elisp-slime-nav
  :diminish (elisp-slime-nav-mode))

(use-package slime)
(use-package slime-company)

(use-package lisp-mode
  ;; Configuration of the built-in lisp-mode.
  :ensure nil
  :commands lisp-mode
  :bind
  (:map emacs-lisp-mode-map
        ("M-." . elisp-slime-nav-find-elisp-thing-at-point)
        ("M-<tab>" . company-complete)
        ("M-?" . describe-function)
        ("C-c C-c" . flycheck-list-errors))
  :config
  (progn
    ;; elint current buffer seems like a fun one.
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    ;; (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'linum-mode)
    (add-hook 'emacs-lisp-mode-hook 'company-mode)
    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))

(use-package highlight-symbol
  :bind (("C-x w ." . highlight-symbol-at-point)
         ("C-x w %" . highlight-symbol-query-replace)
         ("C-x w o" . highlight-symbol-occur)
         ("C-x w c" . highlight-symbol-remove-all)))

(use-package company
  :commands company-mode
  :diminish ""
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :config
  (progn
    (setq company-show-numbers t)))

;;
;; Scala
;;

(use-package scala-mode
  :pin melpa
  :commands scala-mode
  :interpreter ("scala" . scala-mode)
  :config
  (progn
    (add-hook 'scala-mode-hook 'company-mode)
    (add-hook 'scala-mode-hook 'ensime-mode)
    (add-hook 'scala-mode-hook 'linum-mode)))

(use-package sbt-mode
  :pin melpa
  :commands sbt-start sbt-command
  :bind 
  (:map sbt-mode-map
        ("s-k" . comint-clear-buffer)
        ("<up>" . comint-previous-input)
        ("<down>" . comint-next-input))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(add-to-list 'exec-path "/usr/local/bin")

(use-package ensime
  :ensure t
  :pin melpa-stable
  :commands ensime ensime-mode
  :config
  (setq ensime-startup-notification nil
        ensime-startup-snapshot-notification nil)
  :bind
  (:map ensime-mode-map
        ("<tab>" .  nil)
        ("M-<tab>" . ensime-company)
        ("M-?" . ensime-show-doc-for-symbol-at-point)
        ("M-*" . ensime-pop-find-definition-stack)
        ("M-," . ensime-pop-find-definition-stack)
        ("M-." . ensime-edit-definition)
        ("C-c C-t" . ensime-print-type-at-point))
  :config
  (progn
    (setq ensime-sem-high-enabled-p nil)
    (setq ensime-use-helm t)
    (setq ensime-completion-style 'company)))

(use-package sql
  :config
  (progn
    (defun extend-with-mysql-syntax-and-keywords ()
      ;; Make # start a new line comment in SQL.
      (modify-syntax-entry ?# "< b" sql-mode-syntax-table)
      ;; Add a couple of MySQL keywords
      (font-lock-add-keywords
       'sql-mode
       '(("REPLACE" . 'font-lock-keyword-face)
         ("ALGORITHM" . 'font-lock-keyword-face)
         ("MERGE" . 'font-lock-keyword-face))))

    (add-hook 'sql-mode-hook 'linum-mode)
    (add-hook 'sql-mode-hook 'extend-with-mysql-syntax-and-keywords)))

(use-package yaml-mode
  :config
  (progn
    (add-hook 'yaml-mode-hook 'linum-mode)))

(use-package feature-mode
  :bind
  (:map feature-mode-map
        ("M-." . mhj/find-tag)
        ("M-*" . pop-tag-mark))
  :config
  (progn
    (setq feature-indent-level 2)))

(use-package dockerfile-mode
  :config
  (progn
    (add-hook 'dockerfile-mode-hook 'linum-mode)))

(use-package jinja2-mode
  :commands jinja2-mode)

;; Makes it possbile to edit grep buffers!
(use-package wgrep)

;; wgrep support for helm.
(use-package wgrep-helm)

(use-package tex-mode
  :commands tex-mode
  :config
  (progn
    ;; This currently doesn't override the annoying tex-compile
    (define-key tex-mode-map (kbd "C-c C-c") 'compile)
    (define-key latex-mode-map (kbd "C-c C-c") 'compile)))

(use-package glsl-mode)

(use-package shift-text
  :ensure t
  :bind (("M-<up>" . shift-text-up)
         ("M-<down>" . shift-text-down)
         ("M-<left>" . shift-text-left)
         ("M-<right>" . shift-text-right))
  :config
  (progn
    (add-hook 'python-mode-hook (lambda () (setq-local st-indent-step python-indent-offset)))
    (add-hook 'web-mode-hook (lambda () (setq-local st-indent-step web-mode-code-indent-offset)))
    (add-hook 'yaml-mode-hook (lambda () (setq-local st-indent-step 2)))))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :config
  (progn
    (setq whitespace-style '(trailing tabs tab-mark face))
    (global-whitespace-mode)))

(use-package suggest)

(use-package groovy-mode
  :config
  (progn
    (add-hook 'groovy-mode 'linum-mode)))

(use-package window-number
  :init
  (progn

    (autoload 'window-number-mode "window-number"
      "A global minor mode that enables selection of windows according to
 numbers with the C-x C-j prefix.  Another mode,
 `window-number-meta-mode' enables the use of the M- prefix."
   t)

 (autoload 'window-number-meta-mode "window-number"
   "A global minor mode that enables use of the M- prefix to select
 windows, use `window-number-mode' to display the window numbers in
 the mode-line."
   t)

    (window-number-mode 1)
    (window-number-meta-mode 1)))

(use-package osx-dictionary
  ;; Look up a string in the dictionary used by Dictionary.app
  :bind ("M-?" . osx-dictionary-search-global))

(use-package neotree
  :bind (("<f12>" . neotree-projectile))
  :config
  (progn
    (defun neotree-projectile ()
      (interactive)
      (neotree-dir (projectile-project-root)))

    (defun neotree-mode-hook ()
      (interactive)
      (if (projectile-project-p)
          (progn
            (message "color is %s" (car (custom-variable-theme-value 'dired-sidebar-background)))
            (buffer-face-set '(:background "#343d46")))))

    (add-hook 'neotree-mode-hook 'neotree-mode-hook)

    (add-to-list 'neo-hidden-regexp-list "__pycache__")
    (add-to-list 'neo-hidden-regexp-list "\\.egg-info$")

    (setq
     neo-theme 'ascii
     neo-mode-line-type 'none
     neo-show-updir-line nil
     neo-smart-open t
     neo-window-width 30)))

;;; init.el ends here
