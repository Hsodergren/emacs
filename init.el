;; -*- lexical-binding: t; -*-
(package-initialize)

(use-package use-package
  :init
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(use-package emacs
  :preface
  (defun only-window-side-window ()
    (interactive)
    (let ((buffer (current-buffer)))
      (if (not (window-parameter (selected-window) 'window-side))
          (delete-other-windows)
        (delete-window)
        (delete-other-windows)
        (switch-to-buffer buffer))))

  (defun count-lines-region (beg end)
    (interactive "r")
    (let ((end-at-newline (char-equal (char-before end) ?\n))
          (lines  (- (line-number-at-pos end) (line-number-at-pos beg))))
      (message "Number of lines: %d"  (+ lines (if end-at-newline 0 1)))))

  (defun repeatize (keymap)
    (map-keymap (lambda (_kbd cmd)
                  (put cmd 'repeat-map keymap))
                keymap))

  (defun load-work-file ()
    (load-file-if-exists (file-name-concat user-emacs-directory "work.el")))

  (defun load-file-if-exists (file)
    (when (file-exists-p file)
      (load-file file)))

  (defun ansi-color-apply-on-buffer ()
    (interactive)
    (ansi-color-apply-on-region (point-min) (point-max)))

  (defun flash-window (frame)
    (let ((window (frame-selected-window frame)))
      (unless (minibufferp (window-buffer window))
        (pulse-momentary-highlight-region (window-start) (window-end) 'pulse-highlight-face))))

  (defmacro create-scroll-fn (name fn)
    `(defun ,name ()
       (interactive)
       (let ((lines-from-top (count-screen-lines (window-start) (point))))
         (,fn (/ (window-height) 2))
         (move-to-window-line lines-from-top))))
  (create-scroll-fn scroll-up-half scroll-up)
  (create-scroll-fn scroll-down-half scroll-down)

  (defun path-relative-to-user-home-directory (path)
    (file-name-concat (getenv "HOME") path))

  (defun add-to-path (path)
    (add-to-list 'exec-path path)
    (setenv "PATH" (string-join exec-path path-separator)))

  (defun my/add-todo-comment ()
    (interactive)
    (comment-indent)
    (insert "TODO: "))

  :init
  (load-work-file)
  (setq my-global-path-paths (list "~/bin" "~/.local/bin"))

  (when (eq system-type 'darwin)
    (setq my-global-path-paths
          (append my-global-path-paths work-paths))
    (setq mac-command-modifier 'meta))

  (setq auth-sources '((:source "~/.authinfo.gpg")))

  (setq custom-file (expand-file-name "customize.el" user-emacs-directory))
  (load custom-file 'noerror)

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
    (add-to-path (expand-file-name path)))
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
  (setq help-window-select t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 100)
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'window-selection-change-functions 'flash-window)

  (setq kill-region-dwim 'emacs-word)

  (setq treesit-auto-install-grammar t)

  (setq display-buffer-alist
        `((,(rx "*compilation*")
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 1)
           (window-height . 0.33)
           (dedicated . t))
          (,(rx "*eshell*")
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 0.33)
           (dedicated . t)
           (window-parameters . ((no-other-window . t))))
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
  (add-hook 'prog-mode-hook (lambda ()
                              (font-lock-add-keywords
                               nil
                               '(("\\<\\(TODO\\|FIXME\\|HACK\\|NOTE\\|BUG\\):?" 1 font-lock-warning-face t)))))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'after-init-hook 'load-work-file)
  :bind
  ("M-o" . 'other-window)
  ("C-x C-b" . 'ibuffer)
  ("C-v" . 'scroll-up-half)
  ("M-v" . 'scroll-down-half)
  ("M-g i" . 'consult-imenu)
  ("C-M-;" . 'my/add-todo-comment)
  ("M-/" . 'dabbrev-expand))

(use-package repeat
  :init
  (repeat-mode))

(use-package js
  :mode ((rx (or ".morpheme" ".js") eos) . javascript-mode))

(use-package vc
  :config
  (setq vc-git-diff-switches '("-U7"))
  (setq vc-git-log-switches '("--decorate" "--graph" "--format=medium"))
  (setq vc-dir-allow-mass-mark-changes t))

;; :bind (:map vc-git-log-view-mode-map
;;             ("o" . #'log-view-diff-save-excursion)))

(use-package json-mode
  :ensure t)

(use-package diff-mode
  :config
  (keymap-unset diff-mode-map "M-o"))

(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output 'first-error)
  (setq compilation-always-kill t)
  (setq compilation-environment nil)

  (defun my-colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rust-panic
                 "panicked at \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3))

  (add-to-list 'compilation-error-regexp-alist-alist
               '(rust-insta-snapshot
                 "Source: \\([^:\n]+\\):\\([0-9]+\\)"
                 1 2 2))

  (add-to-list 'compilation-error-regexp-alist 'rust-panic)
  (add-to-list 'compilation-error-regexp-alist 'rust-insta-snapshot))

(use-package xref
  :config
  (advice-add 'xref-pulse-momentarily
              :override
              (lambda ()
                (pcase-let ((`(,beg . ,end)
                             (save-excursion
                               (or
                                (let ((length (xref-match-length xref-current-item)))
                                  (and length (cons (point) (+ (point) length))))
                                (back-to-indentation)
                                (if (eolp)
                                    (cons (line-beginning-position) (1+ (point)))
                                  (cons (point) (line-end-position)))))))
                  (pulse-momentary-highlight-region beg end 'pulse-highlight-face)))))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline)))

(use-package yaml-mode)

(use-package disable-mouse)

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

(use-package flymake
  :bind
  (:repeat-map flymake-nav-repeat-map
               ("n" . 'flymake-goto-next-error)
               ("p" . 'flymake-goto-prev-error))
  :bind
  ("C-c n" . 'flymake-goto-next-error)
  ("C-c p" . 'flymake-goto-prev-error))

(use-package sideline
  :init
  (setq sideline-backends-right '(sideline-flymake))
  :hook
  (flymake-mode . sideline-mode))

(use-package sideline-flymake)

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
  ("C-." . #'embark-act))

(use-package consult
  :custom
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip --hidden --glob !.git/**")
  :bind
  ("C-c C-r" . consult-ripgrep))

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


(use-package eglot
  :ensure nil
  :config
  (setq-default eglot-workspace-configuration
                '(:gopls (:hints (:assignVariableTypes t
                                  :compositeLiteralFields t
                                  :compositeLiteralTypes t
                                  :functionTypeParameters t
                                  :parameterNames t
                                  :rangeVariableTypes t
                                  )
                                 :usePlaceholders t)
                         :rust-analyzer (:cargo
                                         (:targetDir t))))
  (setq eglot-stay-out-of '(imenu))
  (setq eglot-code-action-indications nil)
  :hook
  (eglot-managed-mode . (lambda ()
                          (eglot-semantic-tokens-mode -1)))
  :bind
  (:prefix-map
   eglot-prefix
   :prefix "C-,"
   ("a a" . 'eglot-code-actions)
   ("a i" . 'eglot-code-action-organize-imports)
   ("a q" . 'eglot-code-action-quickfix)
   ("f" . 'eglot-format-buffer)
   ("r" . 'eglot-rename)
   ("t" . 'eglot-find-typeDefinition)
   ("c" . 'recompile)))

(use-package org
  :ensure nil
  :init
  (setq org-repeat-map
        (define-keymap
          "M-n" 'org-next-item
          "M-p" 'org-previous-item
          "M-n" 'org-next-item
          "M-p" 'org-previous-item
          "b" 'org-toggle-checkbox))
  :config
  (put 'org-toggle-checkbox 'repeat-map 'org-repeat-map)
  (put 'org-previous-item 'repeat-map 'org-repeat-map)
  (put 'org-next-item 'repeat-map 'org-repeat-map)
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
   ("C-c c" . 'org-capture)
   ("C-c l" . 'org-store-link))
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

(use-package eat
  :config
  (setq eat-term-scrollback-size (* 1024 1024))
  :init
  (define-key eat-eshell-semi-char-mode-map (kbd "M-E") nil))

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

  (defun toggle-eshell-window (arg)
    (interactive "P")
    (let ((buf (current-buffer)))
      (if (string= (buffer-name buf) "*eshell*")
          (when (window-parameter nil 'window-side)
            (delete-window))
        (let ((dir default-directory))
          (eshell)
          (when arg
            (eshell/cd dir)
            (eshell-reset))))))

  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-visual-commands nil)

  :hook
  (eshell-mode . (lambda ()
                   (eat-eshell-mode)))
  :bind
  ("M-E" . 'toggle-eshell-window)
  ("C-x 1" . 'only-window-side-window))

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
  :preface
  (defun my/rust-analyzer-expand-macro ()
    "Expand macro at point using rust-analyzer."
    (interactive)
    (unless (eglot-managed-p)
      (error "Eglot is not managing this buffer"))

    (let* ((server (eglot-current-server))
           (params (eglot--TextDocumentPositionParams))
           (response
            (jsonrpc-request
             server
             "rust-analyzer/expandMacro"
             params)))
      (if-let ((expansion (plist-get response :expansion)))
          (with-current-buffer (get-buffer-create "*rust-analyzer macro expansion*")
            (erase-buffer)
            (insert (format  "// Expansion of %s\n\n" (plist-get response :name)))
            (insert expansion)
            (rust-mode)
            (display-buffer (current-buffer)))
        (message "No expansion available"))))

  :defer t
  :init
  (with-eval-after-load 'compile
    (require 'rust-compile))
  (defun rust-insert-backtrace-dbg ()
    (interactive)
    (insert "dbg!(&std::backtrace::Backtrace::force_capture());"))
  :hook
  (rust-mode . eglot-ensure)
  :bind
  ("C-c C-b" . #'rust-insert-backtrace-dbg)
  ("C-c C-e" . #'my/rust-analyzer-expand-macro))

(use-package typescript-mode)


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

(use-package terraform-mode)

(use-package gruber-darker-theme
  :load-path "lib/gruber-darker-theme"
  :ensure nil
  :demand t
  :config
  (load-theme 'gruber-darker :no-confirm))

(use-package wgsl-mode)

(use-package protobuf-mode
  :preface
  (defun protobuf-insert-next-field-number (arg)
    (interactive "P")
    (let ((current-max 0))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp (rx "=" (* space) (group (+ digit)) (* space) ";") nil t)
          (setq current-max (max (string-to-number (match-string 1)) current-max))))
      (if arg
          (message "%s" current-max)
        (insert (number-to-string (1+ current-max))))))
  :hook
  (protobuf-mode . (lambda () (setq c-basic-offset 4))))

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit,
;; but keep this line
(when (file-exists-p "~/.emacs.d/opam-user-setup.el")
  (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el"))
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
