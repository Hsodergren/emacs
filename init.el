;; funcs
(defun emacs-dir (file)
  (concat user-emacs-directory file))

;; BACKUP FILES
(setq backup-directory-alist `(("." . ,(emacs-dir "saves"))))
(setq backup-by-copying t)

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
  :config
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (setq inhibit-startup-screen t)
  (setq blink-cursor-mode nil)
  (tool-bar-mode -1)
  (global-set-key (kbd "C-c e v") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
  (global-set-key (kbd "C-c s v") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)
  (add-hook `prog-mode-hook (lambda ()
			      (whitespace-cleanup)
			      (show-paren-mode)))
  :custom-face
  (shadow ((t (:foreground "#707070")))))

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

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package yasnippet
  :init
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets")))

(use-package markdown-mode
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . #'er/expand-region))

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
  :bind ("C-." .#'embark-act))

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

(use-package merlin
  :demand t
  :init
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
      (load-file (concat opam-share "/emacs/site-lisp/dune-watch.el")))))

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
