;; funcs
(defun emacs-dir (file)
  (concat user-emacs-directory file))

(setq shell-command-switch "-ic")
(load-file (emacs-dir "funcs.el"))

(add-hook 'after-init-hook (lambda ()
                             (dark-theme)))

;; BACKUP FILES
(setq backup-directory-alist `(("." . ,(emacs-dir "saves"))))
(setq backup-by-copying t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(setq use-package-always-ensure 1)

(define-key global-map (kbd "C-c r") (lambda () (interactive) (recompile)))

(setq custom-file (emacs-dir "settings.el"))
(load custom-file)
(defun package--save-selected-packages (&rest opt) nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook `prog-mode-hook (lambda ()
                            (whitespace-cleanup)
                            (show-paren-mode)))

(add-hook `blackbox-mode-hook (lambda ()
                (define-key blackbox-mode-map (kbd "j") 'bb-down)
                (define-key blackbox-mode-map (kbd "k") 'bb-up)
                (define-key blackbox-mode-map (kbd "h") 'bb-left)
                (define-key blackbox-mode-map (kbd "l") 'bb-right)
                (define-key blackbox-mode-map (kbd "r") 'blackbox)))

(use-package json-mode :ensure t)

(use-package paredit :ensure t
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline)))


;; themes
(use-package gruber-darker-theme
  :ensure nil
  :init
  (add-to-list 'load-path (emacs-dir "lib/gruber-darker-theme")))

(use-package smart-mode-line
  :config
  (sml/setup))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; DISABLE MOUSE
(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package rainbow-mode)
(use-package sideline-flymake)
(use-package sideline
  :config
  (setq sideline-backends-right '(sideline-flymake))
  :hook
  (flymake-mode . sideline-mode))

(use-package fancy-battery
  :init
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package yasnippet
  :init
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))



(use-package compat
  :ensure t)

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
  :ensure t)

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
  (setq tab-always-indent t))

;; bindings
(global-set-key (kbd "C-c e v") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c s v") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (setq magit-diff-refine-hunk t))

(use-package git-timemachine)

(use-package diff-hl
  :init
  (global-diff-hl-mode 1))

(use-package notmuch
  :config
  (setq sendmail-program "/usr/bin/msmtp")
  (setq mail-specify-envelope-from t)
  (setq message-sendmail-envelope-from 'header)
  (setq mail-envelope-from 'header)
  (setq send-mail-function 'message-send-mail-with-sendmail))

(use-package eldoc
  :config
  (global-eldoc-mode)
  (eldoc-schedule-timer))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(use-package eglot :ensure nil
  :config
  (setq-default eglot-workspace-configuration '(:gopls (:hints
                                                        (:assignVariableTypes t
                                                         :compositeLiteralFields t
                                                         :compositeLiteralTypes t
                                                         :functionTypeParameters t
                                                         :parameterNames t
                                                         :rangeVariableTypes t)
                                                        :usePlaceholders t))))

;; PYTHON
(use-package python
  :ensure nil
  :init
  (eglot-ensure))

(use-package go-mode
  :init
  (eglot-ensure))

(use-package org
  :ensure nil
  :config
  (auto-fill-mode)
  (setq fill-column 100))


;; OCAML
;; -- merlin setup ---------------------------------------

(use-package ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))

(use-package tuareg
  :init
  (add-hook 'tuareg-mode (lambda () (progn
                                      (add-hook 'before-save-hook 'ocp-indent-buffer nil 'local))))
  :hook
  (tuareg-mode . eglot-ensure))

(use-package merlin)
(use-package dune)
;; (use-package utop
;;   :config
;;   (setq utop-command "opam config exec -- dune utop . -- -emacs"))
(use-package utop)

;; -- merlin setup ---------------------------------------
(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    ;; (add-hook 'tuareg-mode-hook 'merlin-mode t)
    ;; (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (add-to-list 'exec-path (substring (shell-command-to-string "opam var bin") 0 -1))
    (setenv "PATH" (concat (substring (shell-command-to-string "opam var bin") 0 -1) ":$PATH") t)
    (load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))
    (load-file (concat opam-share "/emacs/site-lisp/dune.el"))
    (load-file (concat opam-share "/emacs/site-lisp/dune-flymake.el"))
    (load-file (concat opam-share "/emacs/site-lisp/dune-watch.el"))))

;; (setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; ;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; ;; by spaces.
;; (define-key merlin-mode-map
;;   (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
;; (define-key merlin-mode-map
;;   (kbd "C-c <down>") 'merlin-type-enclosing-go-down)

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

(use-package dart-server)
(use-package flutter)
(use-package flutter-l10n-flycheck)

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

(use-package protobuf-mode)

(define-derived-mode test-mode nil "test")
(add-to-list 'eglot-server-programs '(test-mode "/home/henrik/programming/ocaml/a_lsp/_build/default/bin/main.exe"))
