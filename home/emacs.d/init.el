;;; init.el --- Mads' configuration file
;;; Commentary:
;;; Code:

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
 shell-file-name "bash")

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
(setenv "PS1" "> ")
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

;; Load my various elisp files.
;; ---------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load "~/.emacs.d/functions.el")
(load custom-file 'noerror)

;; window-system specific configuration
;; ---------------------------------
(if window-system
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      (add-to-list 'initial-frame-alist '(width . 150))
      (add-to-list 'initial-frame-alist '(height . 50))
      (add-to-list 'default-frame-alist '(width . 150))
      (add-to-list 'default-frame-alist '(height . 50))
      (load-theme 'base16-ocean-dark-hartmann t)
      (set-face-attribute 'default nil :font "Menlo-12:antialias=subpixel"))
  (progn
    ;; (load-theme 'basic-light t)
    (menu-bar-mode -1)
    (xterm-mouse-mode t)
    (global-set-key (kbd "C-M-d") 'backward-kill-word)))

;; Configuration of modes
;; ---------------------------------

(use-package flycheck
  ;; On the fly linting.
  :diminish ""
  :bind
  (:map flycheck-mode-map
        ("C-c ! ?" . flycheck-display-error-at-point))
  :commands flycheck-mode
  :config
  (progn
    (flycheck-set-checker-executable 'javascript-eslint "/usr/local/bin/eslint"))
  :init
  (progn
    ;; Disable jshint making eslint the selected linter
    (setq-default flycheck-disabled-checkers '(javascript-jshint))
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (setq flycheck-highlighting-mode 'symbols)
    (setq flycheck-indication-mode 'left-fringe)))

(use-package conf-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.cnf\\'" . conf-mode))))

(use-package compilation
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

(use-package hdl-mode
  ;; My own small package for hdl files.
  :ensure nil
  :load-path "my-pkgs/"
  :mode "\\.hdl\\'"
  :defer)

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

    (setq
     insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls"
     dired-listing-switches "-lXGh --group-directories-first")

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
  ;; TODO: I'd rather not use this I think. But somehow, it seems that flycheck doesn't
  ;;       look in the PATH environment, so this function must be settion some other things.
  :init
  (progn
    (exec-path-from-shell-initialize)))

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
    ;; (setq helm-split-window-in-side-p nil)
    (setq helm-split-window-in-side-p t)
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

(use-package bookmark+
  :bind (("s-<f2>" . bmkp-toggle-autonamed-bookmark-set/delete)
         ("<f2>" . bmkp-next-bookmark-this-buffer)
         ("S-<f2>" . bmkp-previous-bookmark-this-buffer))
  :config
  (progn
    (setq bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
    (setq bmkp-light-left-fringe-bitmap 'empty-line)
    (setq bmkp-auto-light-when-set 'autonamed-bookmark)))

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
    (yas-global-mode)
    (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
    (yas-reload-all)))

(use-package diff-hl
  :init (global-diff-hl-mode))

(use-package org
  :bind
  (:map org-mode-map
        ("C-," . nil)
        ("C-c C-j" . helm-org-in-buffer-headings))
  :init
  (progn
    (require 'ox-publish)
    (require 'ob-ocaml)
    (require 'ob-sh)
    (require 'ob-sql)
    (require 'ob-python)
    (require 'ob-js)
    (require 'ob-R)

    (define-key global-map (kbd "C-c c") 'org-capture)

    (define-key org-mode-map (kbd "C-c C-a") 'org-agenda)
    (define-key org-mode-map (kbd "C-<tab>") nil)

    (setq
     org-html-htmlize-output-type 'css
     ;trying it out
     org-src-fontify-natively t
     org-startup-folded nil
     ;; Living on the edge
     org-confirm-babel-evaluate nil
     org-startup-indented nil
     ;; Don't evaluate on export by default.
     org-export-babel-evaluate nil
     ;; This is important, otherwise I can't tangle source blocks
     ;; written in makefile mode.
     org-src-preserve-indentation t
     org-goto-interface 'outline-path-completion org-goto-max-level 10
     ;; Even if sh-mode source-blocks fail I still want the output.
     org-babel-default-header-args:sh
     '((:prologue . "exec 2>&1") (:epilogue . ":"))

     org-babel-load-languages
          '((ocaml . t)
            (emacs-lisp . t)
            (sh . t)
            (sql . t)
            (makefile . t)
            (python . t)
            (js . t)
            (r . R)))

    (add-hook 'org-mode-hook 'linum-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook (lambda () (imenu-add-to-menubar "Imenu")))

    ;; http://www.wisdomandwonder.com/link/9573/how-to-correctly-enable-flycheck-in-babel-source-blocks
    (defadvice org-edit-src-code (around set-buffer-file-name activate compile)
      (let ((file-name (buffer-file-name)))
        ad-do-it
        (setq buffer-file-name file-name)))))

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

(use-package markdown-mode
  :commands markdown-mode
  :bind
  (:map markdown-mode-map
        ("M-<tab>" . ido-complete-word-ispell)
        ("M-?" . ispell-word)
        ("C-c C-c" . flycheck-list-errors))
  :config
  (progn
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

(use-package octave
  :commands octave-mode
  :mode (("\\.m$" . octave-mode))
  :config
  (progn
    (autoload 'octave-mode "octave-mod" nil t)
    (add-hook 'octave-mode-hook
              (lambda ()
                (abbrev-mode 1)
                (auto-fill-mode 1)
                (if (eq window-system 'x)
                    (font-lock-mode 1))))))

(use-package erlang
  :disabled
  :commands erlang-mode
  :bind
  (:map erlang-mode-map
        ("M-." . erl-find-source-under-point)
        ("M-," . erl-find-source-unwind)
        ("M-<tab>" . erl-complete)
        ("C-c C-c" . compile)
        ("<return>" . newline-and-indent))
  :config
  (progn
    ;; (add-to-list 'load-path "/Users/hartmann/dev/distel/elisp") ; Not in melpa yet
    ;; (require 'distel)
    ;; (distel-setup)
    ;; ;; http://parijatmishra.wordpress.com/2008/08/15/up-and-running-with-emacs-erlang-and-distel/
    ;; ;; http://alexott.net/en/writings/emacs-devenv/EmacsErlang.html#sec8
    ;; (setq inferior-erlang-machine-options '("-sname" "emacs"))
    (add-hook 'erlang-mode-hook 'flycheck-mode)))

(use-package tuareg
  :commands tuareg-mode
  :config
  (progn
    ;; TODO: Consider using flycheck: http://www.flycheck.org/manual/latest/Supported-languages.html#Supported-languages
    ;; TODO: Can I use company-mode for this?

    ;; Add opam emacs directory to the load-path
    (setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
    (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

    ;; Setup environment variables using OPAM
    (dolist (var (car (read-from-string (shell-command-to-string "opam config env --sexp"))))
      (setenv (car var) (cadr var)))

    ;; One of the `opam config env` variables is PATH. Update `exec-path` to that.
    (setq exec-path (split-string (getenv "PATH") path-separator))

    ;; Load merlin-mode
    (require 'merlin)
    (require 'ocp-indent)

    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (setq merlin-use-auto-complete-mode 'easy)

    ;; Automatically load utop.el.
    (add-to-list 'load-path "/Users/hartmann/dev/utop/src/top")
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)

    ;; Used if I want to run some of ISSUU's OCaml projects in UTOP .
    (setenv "AGGREGATOR_CONF_SHADOW" "")
    (setenv "AGGREGATOR_HOME" "/Users/hartmann/dev/backend-insight/aggregator")
    (setenv "PROMOTED_HOME" "/Users/hartmann/dev/backend-promoted")
    (setenv "PROMOTED_CONF_SHADOW" "")

    (define-key merlin-mode-map (kbd "M-<tab>") 'merlin-try-completion)
    (define-key merlin-mode-map "\M-." 'merlin-locate)
    (define-key merlin-mode-map (kbd "C-c C-p") 'prev-match)
    (define-key merlin-mode-map (kbd "C-c C-n") 'next-match)
    (define-key tuareg-mode-map (kbd "C-x C-r") 'tuareg-eval-region)

    ;; (setq merlin-logfile "/Users/hartmann/Desktop/merlin.log")
    (setq merlin-error-after-save t)

    (add-hook 'tuareg-mode-hook
              (lambda ()
                (merlin-mode)
                (utop-minor-mode)
                (define-key utop-minor-mode-map (kbd "C-c C-z") 'utop)
                (setq indent-line-function 'ocp-indent-line)))))

(use-package python
  :commands python-mode
  :bind
  (:map python-mode-map
        ("M-s" . nil)
        ("C-c C-p" . nil)
        ("C-c C-c" . flycheck-list-errors)
        ("M-<tab>" . company-complete))
  :config
  (progn

    (defun flycheck-python-set-pylint-executable ()
      "Use the pylint executable from your local venv."
      (let ((exec-path (python-shell-calculate-exec-path)))
        (setq flycheck-python-pylint-executable (executable-find "pylint"))))

    (defun flycheck-python-setup ()
      "Configure flycheck to use pylint and respect the projects configuration.
       Wait till after the .dir-locals.el has been loaded."
      (add-hook 'hack-local-variables-hook 'flycheck-python-set-pylint-executable nil 'local)
      (setq flycheck-checker 'python-pylint)
      (setq flycheck-pylintrc (concat (upward-find-file "pylint.cfg") "/pylint.cfg")))

    (defun company-python-setup ()
      "Set the relevant backends for company-mode when editing python files."
      (set (make-local-variable 'company-backends)
           '(company-jedi)))

    (add-hook 'python-mode-hook 'flycheck-python-setup)
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'company-mode)
    (add-hook 'python-mode-hook 'linum-mode)
    (add-hook 'python-mode-hook 'company-python-setup)
    (add-hook 'python-mode-hook 'jedi:setup)))

(use-package company-jedi)

(use-package jedi
  :bind
  (:map python-mode-map
        ("M-." . jedi:goto-definition)
        ("M-*" . jedi:goto-definition-pop-marker)
        ("M-?" . jedi:show-doc))
  :init
  (progn
    (setq jedi:complete-on-dot nil)))

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

(use-package elixir-mode
  :commands elixir-mode
  :disabled
  :config
  (progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)))

(use-package alchemist
  :commands alchemist-mode
  :disabled
  :bind
  (:map alchemist-mode-map
        ("M-<tab>" . company-complete)
        ("M-?" . alchemist-help-search-at-point)
        ("C-c C-t" . alchemist-mix-test-file)
        ("C-c C-c" . alchemist-mix-compile)
        ("C-c C-r" . alchemist-mix-run)
        ("C-c C-z" . alchemist-iex-project-run))
  :config
  (progn
    (add-hook 'alchemist-mode-hook 'company-mode)
    (add-hook 'alchemist-iex-mode-hook 'company-mode)))

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
  :pin melpa
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

(use-package elm-mode
  :commands elm-mode)

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
  :disabled
  :init
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
  :disabled t
  :bind (("<f12>" . neotree-projectile))
  :load-path "~/dev/personal/emacs-neotree"
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

    (setq
     neo-theme 'icons
     neo-mode-line-type 'none
     neo-show-updir-line nil
     neo-smart-open t
     neo-window-width 30)))

;;; init.el ends here
