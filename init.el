;; BACKUP FILES
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "saves"))))
(setq backup-by-copying t)
(package-initialize)
(defun path-relative-to-user-home-directory (path)
  (file-name-concat (getenv "HOME") path))

(setq my-global-path-paths (list (path-relative-to-user-home-directory "bin")))

(when (string-equal system-type "darwin")
  (setq my-global-path-paths
        (append my-global-path-paths
                '("/opt/homebrew/lib/ruby/gems/3.4.0/bin/" "~/Library/Android/sdk/platform-tools/")))
  (setq mac-command-modifier 'meta))

(setq auth-sources '((:source "~/.authinfo.gpg")))

(defun add-to-path (path)
  (add-to-list 'exec-path path)
  (setenv "PATH" (string-join (list (getenv "PATH") path) ":")))

(use-package use-package
  :init
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

(use-package json-mode :ensure t)

(use-package paredit :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline)))

(use-package gruber-darker-theme
  :load-path "lib/gruber-darker-theme"
  :ensure nil
  :demand t
  :config
  (load-theme 'gruber-darker :no-confirm))

(use-package emacs
  :init
  (require 'em-tramp)
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
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 100)
  (setq ispell-program-name "aspell"
        ispell-extra-args '("--sug-mode=ultra"))

  (when (display-graphic-p)
    (global-set-key (kbd "C-x C-c")
                    (lambda () (interactive) (message "killing emacs is always a bad idea... I won't allow it!"))))
  (global-set-key (kbd "C-c e v")
                  (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (global-set-key (kbd "C-c s v")
                  (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook `prog-mode-hook 'show-paren-mode)
  (setq comment-auto-fill-only-comments t)
  (add-hook 'prog-mode-hook 'auto-fill-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  :custom-face
  (shadow ((t (:foreground "#707070"))))
  :bind
  ("M-o" . 'other-window)
  ("M-E" . 'eshell))

(use-package yaml-mode)

(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package dired
  :ensure nil
  :config
  (setq dired-listing-switches "-alh")
  (defun dired-kill-ring-save-path ()
    (interactive)
    (if-let ((path (dired-get-filename nil t)))
        (kill-new path)))

  (defun dired-kill-ring-save-path-as-string ()
    (interactive)
    (if-let ((path (dired-get-filename nil t)))
        (kill-new (format "\"%s\"" path)))))


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
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

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
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :ensure t
  :bind
  ("C-." .#'embark-act))

(use-package embark-consult)

(use-package consult-eglot
  :ensure t)

(use-package corfu
  :custom
  (corfu-separator ?\s)
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
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
  ( :prefix-map eglot-prefix
    :prefix "C-,"
    ("a a" . 'eglot-code-actions)
    ("a i" . 'eglot-code-action-organize-imports)
    ("a q" . 'eglot-code-quickfix)
    ("f" . 'eglot-format-buffer)))

;; PYTHON
(use-package python
  :ensure nil
  :init
  (eglot-ensure))

(use-package go-mode
  :init
  (eglot-ensure))

(use-package rust-mode
  :init
  (eglot-ensure))

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
  :config
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (setq eshell-visual-commands nil)

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

  :hook
  (eshell-mode . (lambda ()
                   (set-face-foreground 'eshell-prompt "pink3")
                   (eat-eshell-mode))))

(use-package ellama)
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


;; DART
;; To install dart_language_server run 'pub global activate dart_language_server'
(use-package dart-mode
  :config
  (add-to-list 'exec-path (string-join (list (getenv "HOME") ".flutter" "bin") "/"))
  (setq flutter-sdk-path "~/.flutter/")
  (setq dart-format-on-save t)
  (setq dart-sdk-path "~/.fluttersdk/bin/cache/dart-sdk/")
  :hook
  (dart-mode . eglot-ensure))

(use-package flutter)

;; SCALA
(use-package scala-mode)
;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package erlang)

(use-package zig-mode)

(use-package protobuf-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult-eglot corfu dart-mode diff-hl dired disable-mouse dslide eat
                   eglot-java ellama embark-consult epresent erlang
                   expand-region flutter git-timemachine go-mode
                   json-mode ledger-mode lua-mode magit marginalia
                   markdown-mode ocaml-eglot ocamlformat orderless
                   paredit pdf-tools protobuf-mode rainbow-mode
                   rust-mode sbt-mode scala-mode sideline-flymake
                   tuareg vertico yasnippet zig-mode zoom-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(outline-2 ((t (:inherit custom-state)))))
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
