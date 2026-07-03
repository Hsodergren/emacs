;; -*- lexical-binding: t; -*-
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
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
  (setq my-global-path-paths (list "~/bin" "~/.local/bin" "/usr/local/bin"))
  (load-work-file)
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

  :config
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (setq inhibit-startup-screen t)
  (blink-cursor-mode -1)
  (electric-pair-mode)
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
  (setq delete-pair-blink-delay 0)
  (setq treesit-font-lock-level 4)
  (setq major-mode-remap-alist
        '((python-mode . python-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (rust-mode . rust-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (json-mode . json-ts-mode)))
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
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (setq comment-auto-fill-only-comments t)
  (add-hook 'prog-mode-hook (lambda ()
                              (font-lock-add-keywords
                               nil
                               '(("\\<\\(TODO\\|FIXME\\|HACK\\|NOTE\\|BUG\\):?" 1 font-lock-warning-face t)))))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :bind
  ("M-o" . other-window)
  ("C-c d" . delete-pair)
  ("C-c C-d" . raise-sexp)
  ("C-x C-b" . ibuffer)
  ("C-v" . scroll-up-half)
  ("M-v" . scroll-down-half)
  ("C-M-;" . my/add-todo-comment)
  ("M-/" . dabbrev-expand))

(use-package repeat
  :init
  (repeat-mode))

(use-package js
  :mode ((rx (or ".morpheme" ".js") eos) . javascript-mode)
  :hook
  (javascript-mode . (lambda ()
                       (setq js-indent-level (guess-indent-level)))))

(use-package vc
  :ensure nil
  :init
  (setq vc-git-diff-switches '("-U7"))
  (setq vc-git-log-switches '("--decorate" "--graph" "--format=medium"))
  (setq vc-dir-allow-mass-mark-changes t)
  (setq vc-dir-auto-hide-up-to-date t))

(use-package which-key
  :init
  (setq which-key-idle-delay 2)
  (which-key-mode))

(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c C-s"))
  :bind (:map smerge-basic-map
         ("N" . smerge-vc-next-conflict)
         :repeat-map smerge-repeat-map
         ("N" . smerge-vc-next-conflict)
         ("P" . smerge-prev)))

(use-package yaml-ts-mode
  :mode ((rx (or ".yaml" ".yml") eos)))

(defun my/json-ts-mode-hook-fn ()
  (setq treesit-font-lock-settings
        (treesit-replace-font-lock-feature-settings (treesit-font-lock-rules
                                                     :language 'json
                                                     :feature 'pair
                                                     :override t
                                                     '((pair key: (_) @font-lock-keyword-face)))
                                                    treesit-font-lock-settings))
  (treesit-font-lock-recompute-features))

(use-package json-ts-mode
  :ensure nil
  :hook
  (json-ts-mode . my/json-ts-mode-hook-fn)
  (json-ts-mode . (lambda ()
                    (setq-local json-ts-mode-indent-offset (guess-indent-level)))))

(use-package diff-mode
  :config
  (keymap-unset diff-mode-map "M-o"))

(use-package compile
  :ensure nil
  :init
  (setq compilation-scroll-output 'first-error)
  (setq compilation-always-kill t)
  (setq compilation-environment nil)

  (defun my-colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  :config
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
  :ensure nil
  :init
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
  :ensure nil
  :preface
  (defun toggle-flymake-inline-diagnostics ()
    (interactive)
    (if (null flymake-show-diagnostics-at-end-of-line)
        (setq flymake-show-diagnostics-at-end-of-line 'short)
      (setq flymake-show-diagnostics-at-end-of-line nil))
    (revert-buffer nil t))
  :config
  (setq flymake-show-diagnostics-at-end-of-line 'short)
  :bind
  (:repeat-map flymake-nav-repeat-map
               ("n" . flymake-goto-next-error)
               ("p" . flymake-goto-prev-error))
  :bind
  ("C-c n" . flymake-goto-next-error)
  ("C-c p" . flymake-goto-prev-error)
  ("C-c C-a" . toggle-flymake-inline-diagnostics))

(use-package sideline
  :init
  (setq sideline-backends-right '(sideline-flymake)))

(use-package sideline-flymake)

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package markdown-mode
  :ensure t
  :hook
  (markdown-mode . auto-fill-mode))

(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

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
  ("C-." . embark-act))

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
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (setq magit-diff-refine-hunk t)
  :bind
  ("C-x g" . magit-status))

(use-package git-timemachine)

(use-package diff-hl
  :init
  (global-diff-hl-mode 1))

(use-package eldoc
  :ensure nil
  :config
  (global-eldoc-mode)
  (eldoc-schedule-timer))


(defun imenu-get-pos (item)
  (car (get-text-property 0 'imenu-region item)))

(defun imenu-item-line (item)
  (if-let* ((pos (imenu-get-pos item))
            (kind (get-text-property 0 'imenu-kind item)))
      (save-excursion
        (goto-char pos)

        (let ((i 0))
          (while (and (not (string-search item (thing-at-point 'line)))
                      (< i 5))
            (forward-line)
            (unless (string-match (rx line-start (* whitespace) (literal (string-trim comment-start))) (thing-at-point 'line))
              (setq i (1+ i))))
          (when (>= i 5)
            (goto-char pos)))
        (let* ((ret (concat (string-pad (propertize kind 'face 'shadow) 8) " " (string-trim (thing-at-point 'line)))))
          (add-text-properties 0 1 (text-properties-at 0 item) ret)
          ret))
    item))

(defun filter-imenu-items (items)
  (seq-keep
   (lambda (v)
     (if (seq-contains-p my/eglot-imenu-should-exclude (get-text-property 0 'imenu-kind (car v)))
         nil
       (cond
        ((proper-list-p v)
         (let ((values (filter-imenu-items (cdr v)))
               (line-text (imenu-item-line (car v))))
           (if values
               (cons line-text values)
             (if-let* ((pos (imenu-get-pos (car v))))
                 (cons line-text pos)
               nil))))
        ((consp v)
         (cons (imenu-item-line (car v)) (cdr v))))))
   items))

(ert-deftest test-filter-eglot-imenu ()
  (should (equal (filter-imenu-items '(("foo" . 1) ("bar" . 2) ("baz". 3))) '(("foo" . 1) ("bar" . 2) ("baz". 3))))
  (should (equal (filter-imenu-items `(("foo" . 1) ("bar" . 2) (,(propertize "baz" 'imenu-kind "Constant"). 3))) '(("foo" . 1) ("bar" . 2))))
  (should (equal (filter-imenu-items '(("foo" ("bar" ("baz". 2) ("foobar" . 3))))) '(("foo" ("bar" ("baz". 2) ("foobar" . 3))))))
  (should (equal (filter-imenu-items `(("foo" ("bar" (,(propertize "baz" 'imenu-kind "Constant"). 2) ("foobar" . 3))))) '(("foo" ("bar" ("foobar" . 3))))))
  (should (equal (filter-imenu-items `((,(propertize "foo" 'imenu-region '(1 . 2)) ("bar" (,(propertize "baz" 'imenu-kind "Constant"). 2) (,(propertize "baz" 'imenu-kind "Constant"). 2))))) `(("foo" . 1)))))

(use-package eglot
  :ensure nil
  :preface
  (defvar my/eglot-imenu-should-exclude '("Constant" "Property" "Variable" "Field"))

  (defun my/eglot-imenu (fn)
    (filter-imenu-items (funcall fn)))

  :init
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
  (setq eglot-stay-out-of nil)
  (setq eglot-code-action-indications nil)
  (advice-add 'eglot-imenu :around 'my/eglot-imenu)
  :hook
  (eglot-managed-mode . (lambda ()
                          (eglot-semantic-tokens-mode -1)))
  :bind
  (:prefix-map
   eglot-prefix
   :prefix "C-,"
   ("a a" . eglot-code-actions)
   ("a i" . eglot-code-action-organize-imports)
   ("a q" . eglot-code-action-quickfix)
   ("f" . eglot-format-buffer)
   ("r" . eglot-rename)
   ("t" . eglot-find-typeDefinition)
   ("c" . recompile)))

(use-package project
  :ensure nil
  :bind (:map project-prefix-map ("c" . my/project-compile)))

(use-package org
  :ensure nil
  :init
  (setq org-repeat-map
        (define-keymap
          "M-n" 'org-next-item
          "M-p" 'org-previous-item
          "n" 'org-next-item
          "p" 'org-previous-item
          "b" 'org-toggle-checkbox))
  (setq org-todo-toggle-repeat-map
        (define-keymap
          "t" 'org-todo
          "C-t" 'org-todo))
  :config
  (put 'org-toggle-checkbox 'repeat-map 'org-repeat-map)
  (put 'org-previous-item 'repeat-map 'org-repeat-map)
  (put 'org-next-item 'repeat-map 'org-repeat-map)
  (put 'org-todo 'repeat-map 'org-todo-toggle-repeat-map)
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
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))
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
  (define-key eat-eshell-semi-char-mode-map (kbd "M-E") nil))

(use-package eshell
  :ensure nil
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
  (eshell-mode . eat-eshell-mode)
  :bind
  ("M-E" . toggle-eshell-window)
  ("C-x 1" . only-window-side-window))

(use-package python
  :ensure nil
  :hook
  (python-mode . eglot-ensure))

(use-package go-mode
  :hook
  (go-mode . eglot-ensure))

(use-package rust-ts-mode
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
      (if-let ((expansion (plist-get response :expansion))
               (name (plist-get response :name)))
          (with-current-buffer (get-buffer-create "*rust-analyzer macro expansion*")
            (erase-buffer)
            (insert (format  "// Expansion of %s\n\n" name))
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

  (defun rust-project-fmt ()
    (interactive)
    (let* ((default-directory (project-root (project-current)))
           (process (start-process "cargofmt" nil "cargo" "fmt")))
      (set-process-sentinel process (lambda (proc event)
                                      (when (and (string-match (rx (or "finished" "exited")) event)
                                                 (/= (process-exit-status proc) 0))
                                        (error "cargo fmt failed"))))))
  :hook
  (rust-ts-mode . eglot-ensure)
  :bind
  ("C-c C-b" . rust-insert-backtrace-dbg)
  ("C-c C-e" . my/rust-analyzer-expand-macro)
  ("C-c C-f" . rust-project-fmt))

(defun guess-indent-level ()
  (let ((indent-level 4))
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp (rx bol (group (+ whitespace))) nil t)
        (setq indent-level (length (match-string 1))))
      indent-level)))

(use-package typescript-ts-mode
  :ensure nil
  :hook
  (typescript-ts-mode . eglot-ensure)
  (typescript-ts-mode . (lambda ()
                          (setq-local typescript-ts-indent-offset (guess-indent-level)))))


(use-package ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

(use-package tuareg
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

(defvar my/project-compile-options nil)

(defun my/candidate-to-command (cand)
  (cond*
   ((stringp cand) cand)
   ((plistp cand)
    (let ((env (string-join
                (mapcar (lambda (env)
                          (format "%s=%s" (car env) (cdr env)))
                        (plist-get cand :env))
                " "))
          (prefix (or (plist-get cand :prefix) ""))
          (suffix (or (plist-get cand :suffix) ""))
          (cmd (plist-get cand :cmd))
          (cmds (plist-get cand :cmds))
          (cmds-separator (or (plist-get cand :cmds-separator) "&&")))
      (cond
       (cmd (string-join (seq-filter (lambda (str) (not (string-empty-p str)))
                                     (list (propertize env 'face 'shadow)
                                           (propertize prefix 'face 'shadow)
                                           cmd
                                           (propertize suffix 'face 'shadow)))
                         " "))
       (cmds
        (string-join (mapcar 'my/candidate-to-command cmds) (format " %s " cmds-separator)))
       (t (error "cmd or cmds needs to be given in a plist")))))))

(ert-deftest test-candidate-to-command ()
  (should (equal (my/candidate-to-command "asd") "asd"))
  (should (equal (my/candidate-to-command '(:cmd  "asd")) "asd"))
  (should (equal (my/candidate-to-command '(:env (("foo" . "bar")) :cmd "baz")) "foo=bar baz"))
  (should (equal (my/candidate-to-command '(:env (("foo" . "bar")) :cmd "baz" :prefix "foobar" :suffix "barbaz"))
                 "foo=bar foobar baz barbaz"))
  (should (equal (my/candidate-to-command '(:cmds ("foo" (:cmd "bar" :env (("foo" . "bar")))))) "foo && foo=bar bar")))

(defun my/project-compile ()
  (interactive)
  (if-let* ((project (project-current))
            (name (project-name project))
            (options (alist-get name my/project-compile-options nil nil #'string-match))
            (default-directory (project-root project)))
      (let* ((candidates (if (functionp options)
                             (funcall options)
                           options))
             (final (mapcar #'my/candidate-to-command candidates))
             (command (completing-read "Compile project: " final)))
        (compile command))
    (call-interactively 'project-compile)))


(defvar my/stored-frame-size `(:width ,(frame-pixel-width)
                               :height ,(frame-pixel-height)
                               :x ,(car  (frame-position))
                               :y ,(cdr (frame-position))))
(defvar my/current-frame-resize-mode 'custom)
(defun my/update-frame-size (&rest args)
  (let* ((current-width (frame-pixel-width))
         (current-height (frame-pixel-height))
         (current-pos (frame-position))
         (current-x (car current-pos))
         (current-y (cdr current-pos)))
    (unless (and (= current-width (plist-get my/stored-frame-size :width))
                 (= current-height (plist-get my/stored-frame-size :height))
                 (= current-x (plist-get my/stored-frame-size :x))
                 (= current-y (plist-get my/stored-frame-size :y)))
      (setq my/current-frame-resize-mode 'custom
            my/stored-frame-size `(:width ,current-width :height ,current-height :x ,current-x :y ,current-y)))))

(defun my/set-custom-frame-mode (&rest args)
  (setq my/current-frame-resize-mode 'custom))

(add-hook 'window-size-change-functions #'my/set-custom-frame-mode)
(add-hook 'move-frame-functions #'my/set-custom-frame-mode)

(defun my/toggle-frame-position ()
  (interactive)
  (let* ((monitor (frame-monitor-workarea))
         (zero-width (car monitor))
         (zero-height (cadr monitor))
         (display-width (caddr monitor))
         (display-height (cadddr monitor)))
    (cond
     ((eq my/current-frame-resize-mode 'custom)
      (message "setting right")
      (my/update-frame-size)
      (setq my/current-frame-resize-mode 'right)
      (set-frame-size nil (/ display-width 2) display-height t)
      (set-frame-position nil (/ display-width 2) zero-height))
     ((eq my/current-frame-resize-mode 'right)
      (message "setting left")
      (setq my/current-frame-resize-mode 'left)
      (set-frame-size nil (/ display-width 2) display-height t)
      (set-frame-position nil zero-width zero-height))
     ((eq my/current-frame-resize-mode 'left)
      (message "setting custom")
      (setq my/current-frame-resize-mode 'custom)
      (set-frame-size nil (plist-get my/stored-frame-size :width) (plist-get my/stored-frame-size :height) t)
      (set-frame-position nil (plist-get my/stored-frame-size :x) (plist-get my/stored-frame-size :y)))
     (t (message "nope")))))
