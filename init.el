(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "79485bab8bb220562d4acd003e4b6f1c9005af41e91f81b7a0e89b7e3a301203" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "33af2d5cb040182b798c3a4ee6d16210e700a2fabaa409231e1c4a003cafd1c2" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "49113d0c7d23c1f752409d0d4d34d3f5af4f1692f5dd6d1b1b3c805ca2d606aa" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default))
 '(dired-dwim-target t)
 '(dired-use-ls-dired t)
 '(eldoc-idle-delay 0.2)
 '(flycheck-display-errors-delay 0.4)
 '(frame-brackground-mode 'dark)
 '(helm-M-x-reverse-history nil)
 '(helm-completion-style 'emacs)
 '(org-agenda-files
   '("~/programming/coursera/machine_learning_stanford/machine_learning.org"))
 '(org-fontify-done-headline t)
 '(package-selected-packages
   '(sicp rg zoom-window yasnippet utop use-package tuareg transmission toml-mode smartparens smart-mode-line racket-mode racer proof-general pdf-tools merlin-eldoc key-chord jinja2-mode helm-rg helm-projectile gruber-darker-theme go-eldoc git-timemachine fzf flymake-cursor flycheck-rust flutter-l10n-flycheck fancy-battery evil-magit evil-collection eglot dune disable-mouse diff-hl dart-server dart-mode company-go company-anaconda avy auto-compile))
 '(ring-bell-function 'ignore)
 '(transmission-host "192.168.10.118"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :foundry "ADBO" :family "Source Code Pro"))))
 '(merlin-type-face ((t (:inherit caml-types-expr-face :background "dim gray")))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq tab-width 4)
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(setq use-package-always-ensure t)
(setq load-prefer-newer t)

(add-hook `prog-mode-hook (lambda ()
			    (whitespace-cleanup)
			    (show-paren-mode)))

(add-hook `blackbox-mode-hook (lambda ()
				(define-key blackbox-mode-map (kbd "j") 'bb-down)
				(define-key blackbox-mode-map (kbd "k") 'bb-up)
				(define-key blackbox-mode-map (kbd "h") 'bb-left)
				(define-key blackbox-mode-map (kbd "l") 'bb-right)
				(define-key blackbox-mode-map (kbd "r") 'blackbox)))

;; BACKUP FILES
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)

(use-package gruber-darker-theme
  :defer t
  :init (load-theme 'gruber-darker t))

(add-hook 'prog-mode-hook 'linum-mode)

;; DISABLE MOUSE
(use-package disable-mouse
  :init
  (global-disable-mouse-mode))

(use-package fancy-battery
  :init
  (add-hook 'after-init-hook #'fancy-battery-mode))

(use-package zoom-window
  :bind
  ("M-z" . zoom-window-zoom))

(use-package flymake-cursor)

(use-package fzf)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package yasnippet
  :init
  (yas-global-mode))

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
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x C-r" . helm-recentf)))

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

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package projectile
  :init
  (projectile-mode t)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map))

(use-package smartparens)

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

;; DIRED
(setq dired-listing-switches "-alh")

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


(use-package jinja2-mode)
;; OCAML
(use-package tuareg
  :bind (:map evil-normal-state-map
	      ("K" . 'merlin-document)))

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
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'company-mode)
(add-hook 'merlin-mode-hook 'merlin-eldoc-setup)
(setq merlin-error-after-save t)

;; -- merlin setup ---------------------------------------

(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

;; -- enable auto-complete -------------------------------
;; Not required, but useful along with merlin-mode
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(load-file (concat opam-share "/emacs/site-lisp/ocp-indent.el"))

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
  (define-key evil-insert-state-map (kbd "C-return") 'octave-send-line)
  )
(add-hook 'octave-mode-hook 'my/octave-hook)
