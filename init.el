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
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default)))
 '(flycheck-display-errors-delay 0.0)
 '(package-selected-packages
   (quote
    (helm-projectile projectile disable-mouse yasnippet fzf pdf-tools flycheck-rust toml-mode helm-ls-git helm-find helm-find-files company-jedi company-go go-mode company-mode-go gruber-darker-theme evil-collection helm help racer python-mode rust-mode flycheck evil-magit magit company auto-compile use-package key-chord evil)))
 '(ring-bell-function (quote ignore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:foreground "tan4")))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(use-package gruber-darker-theme)
(load-theme 'gruber-darker)
(setq tab-width 4)
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)

(add-hook 'prog-mode-hook 'linum-mode)

;; BACKUP FILES
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

;; DISABLE MOUSE
(use-package disable-mouse)
(global-disable-mouse-mode)

(use-package fzf)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package yasnippet)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  :config
  (evil-mode 1))

(use-package evil-collection)
(evil-collection-init)

(use-package key-chord)
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)

;; bindings
(global-set-key (kbd "C-c e v") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

;; evil mode bindings
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "L" 'evil-next-buffer)
(define-key evil-normal-state-map "H" 'evil-prev-buffer)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "M-/") 'company-complete-common)

(use-package magit
  :bind
  ("C-x g" . magit-status)

  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq magit-push-always-verify nil)
  (setq git-commit-summary-max-length 50)
  (with-eval-after-load 'magit-remote
    (magit-define-popup-action 'magit-push-popup ?P
                               'magit-push-implicitly--desc
                               'magit-push-implicitly ?p t))
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package flycheck)
(use-package eldoc)
(use-package org)
(use-package smart-mode-line)
(sml/setup)
(evil-ex-define-cmd "ls" 'helm-mini)
(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  :config
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x C-r" . helm-recentf)))

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


(use-package pdf-tools)
(pdf-tools-install)
;; RUST
(use-package flycheck-rust)
(use-package toml-mode)
(use-package racer)
(use-package rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; PYTHON
(use-package python-mode)
(use-package company-jedi)
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; GO
(use-package company-go)
(use-package go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook (lambda()
			  (setq indent-tabs-mode 1)
			  (setq tab-width 4)))

(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go)) (company-mode)))

