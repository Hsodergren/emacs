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
    ("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "79485bab8bb220562d4acd003e4b6f1c9005af41e91f81b7a0e89b7e3a301203" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "33af2d5cb040182b798c3a4ee6d16210e700a2fabaa409231e1c4a003cafd1c2" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "49113d0c7d23c1f752409d0d4d34d3f5af4f1692f5dd6d1b1b3c805ca2d606aa" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default)))
 '(dired-dwim-target t)
 '(dired-use-ls-dired t)
 '(flycheck-display-errors-delay 0.0)
 '(helm-completion-style (quote emacs))
 '(package-selected-packages
   (quote
    (anaconda-mode company-anaconda ess-R-data-view ess-view ess tuareg eglot heaven-and-hell grandshell-theme gotham-theme flucui-themes faff-theme afternoon-theme arc-dark-theme abyss-theme jsonrpc proof-general merlin-eldoc utop dune flymake-cursor elpy go-rename guru-mode go-guru go-eldoc jinja2-mode diff-hl git-gutter top-mode helm-projectile projectile disable-mouse yasnippet fzf pdf-tools flycheck-rust toml-mode helm-ls-git helm-find helm-find-files company-jedi company-go go-mode company-mode-go gruber-darker-theme evil-collection helm help racer rust-mode flycheck evil-magit magit company auto-compile use-package key-chord evil)))
 '(ring-bell-function (quote ignore)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:foreground "tan4"))))
 '(merlin-eldoc-occurrences-face ((t (:background "gray18")))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq tab-width 4)
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(setq use-package-always-ensure t)
(setq load-prefer-newer t)

;; BACKUP FILES
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)

(use-package gruber-darker-theme)
(load-theme 'gruber-darker)

(add-hook 'prog-mode-hook 'linum-mode)

;; DISABLE MOUSE
(use-package disable-mouse)
(global-disable-mouse-mode)

(use-package flymake-cursor)

(use-package fzf)
(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

(use-package yasnippet
  :init
  (yas-global-mode))

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

(use-package avy)

;; evil mode bindings
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-define evil-normal-state-map ",q" 'evil-delete-buffer)
(define-key evil-normal-state-map "\C-j" 'evil-window-down)
(define-key evil-normal-state-map "\C-k" 'evil-window-up)
(define-key evil-normal-state-map "\C-h" 'evil-window-left)
(define-key evil-normal-state-map "\C-l" 'evil-window-right)
(define-key evil-normal-state-map "L" 'evil-next-buffer)
(define-key evil-normal-state-map "H" 'evil-prev-buffer)
(define-key evil-normal-state-map "f" 'avy-goto-word-1)

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
  )


(use-package diff-hl
  :init
  (global-diff-hl-mode 1))

(use-package flycheck)
(use-package eldoc
  :init
  (global-eldoc-mode))
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
;; LISP
(use-package cl)
;; RUST
(use-package flycheck-rust)
(use-package toml-mode)
(use-package racer)
(use-package rust-mode)
(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'flycheck-mode)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; PYTHON
(use-package anaconda-mode)
(use-package company-anaconda)

(eval-after-load "company" '(add-to-list 'company-backends 'company-anaconda))
(add-hook 'python-mode-hook 'anaconda-mode)

;; GO
(use-package company-go)
(use-package go-mode)
(use-package go-eldoc)
(use-package go-rename)
(use-package go-guru)

(defun my/go-mode-hook ()
  (setq indent-tabs-mode 1)
  (setq tab-width 4)
  (load-file "$GOPATH/src/golang/x/tools/cmd/guru/go-guru.el")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (set (make-local-variable 'company-backends) '(company-go))
  (company-mode)
  (go-eldoc-setup)
  )

(add-hook 'go-mode-hook 'my/go-mode-hook)


(use-package jinja2-mode)
;; OCAML
(use-package tuareg)
(use-package dune)
(use-package merlin)
(use-package merlin-eldoc)
(use-package utop)
(use-package proof-general)

(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
(autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'tuareg-mode-hook 'company-mode)
(add-hook 'merlin-mode 'merlin-eldoc-setup)
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
