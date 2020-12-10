;; funcs
(defun emacs-dir (file)
  (concat user-emacs-directory file))

(setq shell-command-switch "-ic")
(load-file (emacs-dir "funcs.el"))

(add-hook 'after-init-hook (lambda ()
                             (time-sensitive-theme)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(define-key global-map (kbd "C-x C-c") (lambda () (interactive) (message "QUIT disabled, use ':q'")))

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

(use-package csv-mode)
(use-package json-mode)

(use-package geiser)
(use-package paredit
  :hook ((lisp-mode emacs-lisp-mode scheme-mode) . paredit-mode)
  :bind (:map lisp-mode-map       ("<return>" . paredit-newline))
  :bind (:map scheme-mode-map     ("<return>" . paredit-newline))
  :bind (:map emacs-lisp-mode-map ("<return>" . paredit-newline)))

;; BACKUP FILES
(setq backup-directory-alist `(("." . ,(emacs-dir "saves"))))
(setq backup-by-copying t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)

;; themes
(use-package gruber-darker-theme
  :ensure nil
  :init
  (add-to-list 'load-path (emacs-dir "lib/gruber-darker-theme")))

(use-package smart-mode-line
  :config
  (sml/setup))

(add-hook 'prog-mode-hook 'linum-mode)

;; DISABLE MOUSE
(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package rainbow-mode)

(use-package fancy-battery
  :init
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package flymake-cursor)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package yasnippet
  :init
  (yas-global-mode)
  (setq yas-snippet-dirs '("~/emacs.d/snippets")))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.2))

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (evil-ex-define-cmd "ls" 'helm-mini)
  :bind (:map evil-normal-state-map
          (",q" . 'kill-current-buffer)
          ("C-j" . 'evil-window-down)
          ("j" . 'my-next-line)
          ("k" . 'my-prev-line)
          ("C-k" . 'evil-window-up)
          ("C-h" . 'evil-window-left)
          ("C-l" . 'evil-window-right)
          ("L" . 'evil-next-buffer)
          ("H" . 'evil-prev-buffer)
          ("C-c C-c" .'compile)))

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  :config
  (helm-mode 1)
  :bind (("M-x" . 'helm-M-x)
         ("C-x C-f" . 'helm-find-files)
         ("C-x C-b" . 'helm-buffers-list)
         ("C-x C-r" . 'helm-recentf)))

;; bindings
(global-set-key (kbd "C-c e v") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c s v") (lambda () (interactive) (load-file "~/.emacs.d/init.el")))

(use-package avy
  :bind (:map evil-normal-state-map
     ("f" . 'avy-goto-word-1)))

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("M-/" . 'company-complete-common))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (setq magit-diff-refine-hunk t))

(use-package git-timemachine
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(use-package diff-hl
  :init
  (global-diff-hl-mode 1))

(use-package flycheck)
(use-package eldoc
  :init
  (global-eldoc-mode)
  (eldoc-schedule-timer))


(use-package projectile
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package helm-projectile
  :init
  (helm-projectile-on)
  :bind
  (("C-x C-g" . helm-projectile)
   ("C-x p r" . helm-projectile-recentf)))
;;(setq help-mode-fuzzy-match t)
;;(setq helm-completion-in-region-fuzzy-match t)
(setq helm-grep-ag-command "rg --color=always --colors 'match:fg:red' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:red'" "--colors 'match:bg:yellow'"))

;;(add-to-list 'load-path "~/.emacs.d/emacs-application-framework")
;;(require 'eaf)

(use-package pdf-tools
  :config
  (pdf-tools-install))

;; RUST
(use-package flycheck-rust)
(use-package toml-mode)
(use-package racer)
(use-package rust-mode
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

;; PYTHON
(use-package anaconda-mode)
(use-package company-anaconda)

(eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)

;; GO
(use-package company-go)
(use-package go-mode)

(use-package go-eldoc)

(defun my/go-mode-hook ()
  (setq indent-tabs-mode 1)
  (setq tab-width 4)
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (go-eldoc-setup))

(add-hook 'go-mode-hook 'my/go-mode-hook)


;; OCAML
(use-package tuareg)

(use-package dune)
(use-package merlin)

(use-package merlin-eldoc)
(use-package utop)
(use-package proof-general)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode)
                ("\\.atd$" . tuareg-mode))
              auto-mode-alist))
;; (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-hook 'tuareg-mode-hook 'company-mode t)
(add-hook 'merlin-mode-hook 'merlin-eldoc-setup)
(setq merlin-error-after-save t)
;; :bind (:map tuareg-mode-map ("K" . merlin-document)))


;; -- merlin setup ---------------------------------------

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    (evil-define-key 'normal merlin-mode-map (kbd "K") 'merlin-document)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    (add-to-list 'exec-path (substring (shell-command-to-string "opam config var bin") 0 -1))
    (load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))))

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
(use-package eglot)
(use-package dart-mode)
(use-package dart-server)
(use-package flutter)
(use-package flutter-l10n-flycheck)
(defun my/dart-hook ()
  (define-key evil-normal-state-map (kbd "C-c C-r") 'flutter-run-or-hot-reload)
  (setq flutter-sdk-path "~/.fluttersdk/")

  (setq dart-format-on-save t)
  (setq dart-sdk-path "~/.fluttersdk/bin/cache/dart-sdk/")
  (eglot)
  (add-to-list 'eglot-server-programs '(dart-mode . ("dart_language_server"))))
(add-hook 'dart-mode-hook 'my/dart-hook)
(add-hook 'dart-mode-hook 'eglot-ensure)
(add-hook 'dart-mode-hook 'dart-server-hook)

;; OCTAVE
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
(defun my/octave-hook ()
  (define-key evil-normal-state-map (kbd "C-return") 'octave-send-line)
  (define-key evil-insert-state-map (kbd "C-return") 'octave-send-line))
(add-hook 'octave-mode-hook 'my/octave-hook)

(load-file (emacs-dir "post.el"))
