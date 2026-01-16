
(package-initialize)
(defun path-relative-to-user-home-directory (path)
  (file-name-concat (getenv "HOME") path))

(setq my-global-path-paths (list (path-relative-to-user-home-directory "bin")))

(when (string-equal system-type "darwin")
  (setq my-global-path-paths
        (append my-global-path-paths
                '("/opt/homebrew/lib/ruby/gems/3.4.0/bin/" "~/Library/Android/sdk/platform-tools/" "~/Library/Android/sdk/emulator")))
  (setq mac-command-modifier 'meta))

(setq auth-sources '((:source "~/.authinfo.gpg")))

(defun add-to-path (path)
  (add-to-list 'exec-path path)
  (setenv "PATH" (string-join (list (getenv "PATH") path) ":")))

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(load custom-file 'noerror)

(use-package use-package
  :init
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(use-package emacs
  :init
  (require 'em-tramp)
  (setq frame-resize-pixelwise t)
  (when window-system
    (if (> (/ (float  (x-display-pixel-width)) (x-display-pixel-height)) 2)
        (setq default-frame-alist
              `((top . 0) (left . ,(/ (x-display-pixel-width) 4))
                (width . (text-pixels . ,(/ (x-display-pixel-width) 2)))
                (height . (text-pixels . ,(x-display-pixel-height)))))
      (setq default-frame-alist
              `((top . 0) (left . 0)
                (width . (text-pixels . ,(/ (x-display-pixel-width) 2)))
                (height . (text-pixels . ,(x-display-pixel-height)))))))
  (setq password-cache t
        password-cache-expiry 600)
  (setq-default tab-width 4)
  (setq ring-bell-function 'ignore)
  (dolist (path my-global-path-paths)
    (add-to-path path))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

  :config
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq inhibit-startup-screen t)
  (setq blink-cursor-mode nil)
  (setq split-width-threshold 200)
  (setq split-height-threshold nil)
  (setq eldoc-print-after-edit t)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq backup-directory-alist `(("." . ,(file-name-concat user-emacs-directory "saves"))))
  (setq backup-by-copying t)
  (setq-default imenu-flatten t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 100)
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))

  (setq display-buffer-alist
        `((,(rx "*compilation*")
           (display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.33)
           (dedicated . t))
          (,(rx "*eshell*")
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-height . 0.33)
           (dedicated . t)
           (window-parameters . ((no-other-window . t))))
          (,(rx "*eldoc*")
           (display-buffer-at-bottom)
           (window-height . 10))
          (,(rx "*Flymake diagnostics " (* any) "*")
           (display-buffer-at-bottom)
           (window-height . 4))))

  (when (display-graphic-p)
    (global-set-key (kbd "C-x C-c")
                    (lambda () (interactive) (message "killing emacs is always a bad idea... I won't allow it!"))))
  (global-set-key (kbd "C-c e v")
                  (lambda () (interactive) (find-file (file-name-concat user-emacs-directory "init.el"))))
  (global-set-key (kbd "C-c s v")
                  (lambda () (interactive) (load-file (file-name-concat user-emacs-directory "init.el"))))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook `prog-mode-hook 'show-paren-mode)
  (setq comment-auto-fill-only-comments t)
  (add-hook 'prog-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :bind
  ("M-o" . 'other-window)
  ("C-x C-b" . 'ibuffer))

(use-package json-mode
  :ensure t)

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline)))

(use-package yaml-mode)

(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package dired
  :ensure nil
  :preface
  (defun dired-kill-ring-save-path ()
    (interactive)
    (if-let ((path (dired-get-filename nil t)))
        (kill-new path)))
  (defun dired-kill-ring-save-path-as-string ()
    (interactive)
    (if-let ((path (dired-get-filename nil t)))
        (kill-new (format "\"%s\"" path))))
  :config
  (setq dired-listing-switches "-alh"))

(use-package rainbow-mode)

(use-package sideline-flymake)
(use-package sideline
  :config
  (setq sideline-backends-right '(sideline-flymake))
  :hook
  (flymake-mode . sideline-mode))

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package yasnippet
  :init
  (yas-global-mode)
  (setq yas-snippet-dirs (file-name-concat user-emacs-directory "snippets")))

(use-package markdown-mode
  :ensure t
  :init
  (auto-fill-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . #'er/expand-region))

(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))
        orderless-component-separator "[ ,]"))

(use-package embark
  :ensure t
  :bind
  ("C-." .#'embark-act))

(use-package embark-consult)

(use-package consult-eglot
  :ensure t)

(use-package corfu
  :custom
  (setq corfu-separator ?\s)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.2)
  (setq corfu-cycle t)
  (setq corfu-preselect 'prompt)
  :init
  (global-corfu-mode)
  (keymap-set corfu-map "RET" #'corfu-send)
  (setq tab-always-indent t))

(use-package magit
  :config
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (setq magit-diff-refine-hunk t)
  :bind
  ("C-x g" . #'magit-status))

(use-package git-timemachine)

(use-package diff-hl
  :init
  (global-diff-hl-mode 1))

(use-package eldoc
  :config
  (global-eldoc-mode)
  (eldoc-schedule-timer))

(use-package eglot :ensure nil
  :config
  (setq-default eglot-workspace-configuration '(:gopls (:hints
                (:assignVariableTypes t
                 :compositeLiteralFields t
                 :compositeLiteralTypes t
                 :functionTypeParameters t
                 :parameterNames t
                 :rangeVariableTypes t)
                :usePlaceholders t)))
  (setq eglot-stay-out-of '(imenu))
  :bind
  (:prefix-map
   eglot-prefix
   :prefix "C-,"
   ("a a" . 'eglot-code-actions)
   ("a i" . 'eglot-code-action-organize-imports)
   ("a q" . 'eglot-code-action-quickfix)
   ("f" . 'eglot-format-buffer)))

(use-package python
  :ensure nil
  :init
  :hook
  (python-mode . eglot-ensure))

(use-package go-mode
  :init
  :hook
  (go-mode . eglot-ensure))

(use-package rust-mode
  :init
  :hook
  (rust-mode . eglot-ensure)
  (rust-mode . (lambda ()
                 (add-hook 'before-save-hook 'eglot-format-buffer nil t))))

(use-package org
  :ensure nil
  :config
  (setq org-tags-column 80)
  (setq org-default-notes-file (file-name-concat user-emacs-directory "org" "notes.org"))
  (setq org-directory (file-name-concat user-emacs-directory "org"))
  (setq org-agenda-files '("todo.org"))
  (setq org-capture-templates
        `(("t" "Task" entry (file+headline "todo.org" "Todo")
           "* TODO %? %^g"
           :prepend t
           :empty-lines-after 1)
          ("q" "Question" entry (file+headline "todo.org" "Questions")
           "* UNANSWERED %? %^g"
           :prepend t
           :empty-lines-after 1)
          ("c" "Calendar item")
          ("cs" "Single (Single day event)" entry (file "calendar.org")
           "* %? \n%^t")
          ("cr" "Range (Multiple day event)" entry (file "calendar.org")
           "* %? \n%^{From:}t--%^{To:}t")))
  (setq calendar-week-start-day 1)
  :bind
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture))
  :hook
  (org-mode . auto-fill-mode))

(use-package ledger-mode
  :config
  (setq ledger-reports
        '(("bal" "%(binary) -f %(ledger-file) bal --no-color")
          ("reg" "%(binary) -f %(ledger-file) reg --no-color")
          ("payee" "%(binary) -f %(ledger-file) reg @%(payee) --no-color")
          ("account" "%(binary) -f %(ledger-file) reg %(account) --no-color")))
  :mode "\\.dat")

(use-package eat)

(use-package eshell
  :demand t
  :preface
  (defun myeshell/find-exact-match (full-path match-p)
    (let ((current-dir (file-name-nondirectory (directory-file-name full-path)))
          (parent-dir (file-name-parent-directory full-path)))
      (when parent-dir
        (if (funcall match-p current-dir)
            full-path
          (myeshell/find-exact-match parent-dir match-p)))))

  (defun eshell/cdd (arg)
    (if-let ((path (or (myeshell/find-exact-match default-directory (lambda (dir) (string-equal arg dir)))
                       (myeshell/find-exact-match default-directory (lambda (dir) (string-match-p arg dir))))))
        (eshell/cd path)
      (error "Cannot find parent directory '%s'" arg)))
  (defun toggle-eshell-window ()
    (interactive)
    (let ((buf (current-buffer)))
      (if (and
           (string= (buffer-name buf) "*eshell*"))
          (when (window-parameter nil 'window-side)
            (delete-window))
        (eshell))))

  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-visual-commands nil)

  :hook
  (eshell-mode . (lambda ()
                   (eat-eshell-mode)))
  :bind
  ("M-E" . 'toggle-eshell-window))

(use-package terraform-mode)

(use-package ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

(use-package tuareg
  :init
  (add-hook 'tuareg-mode (lambda () (progn
                                      (add-hook 'before-save-hook 'ocp-indent-buffer nil 'local))))
  :hook
  (tuareg-mode . eglot-ensure)
  (tuareg-mode . ocaml-eglot))

(use-package ocaml-eglot
  :ensure  t
  :after tuareg)

(use-package dart-mode
  :config
  (add-to-list 'exec-path (string-join (list (getenv "HOME") ".flutter" "bin") "/"))
  (setq flutter-sdk-path "~/.flutter/")
  (setq dart-format-on-save t)
  (setq dart-sdk-path "~/.fluttersdk/bin/cache/dart-sdk/")
  :hook
  (dart-mode . eglot-ensure))

(use-package flutter)

(use-package scala-mode)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package erlang)

(use-package zig-mode)

(use-package gruber-darker-theme
  :load-path "lib/gruber-darker-theme"
  :ensure nil
  :demand t
  :config
  (load-theme 'gruber-darker :no-confirm))

(use-package protobuf-mode)

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit,
;; but keep this line
(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
