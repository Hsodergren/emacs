(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
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
 '(custom-safe-themes
   (quote
    ("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default)))
 '(package-selected-packages
   (quote
    (gruber-darker-theme evil-collection helm help racer python-mode rust-mode flycheck evil-magit magit company auto-compile use-package key-chord evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(menu-bar-mode -1)
(tool-bar-mode -1)
(global-linum-mode t)
(package-install 'gruber-darker-theme)
(load-theme 'gruber-darker)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode))

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
(use-package helm
  :config
  (helm-mode 1))


;; RUST
(use-package rust-mode)
(setq rust-format-on-save t)
(use-package racer)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

;; PYTHON
(use-package python-mode)

(setq tab-width 4)
(setq inhibit-splash-screen t
      inhibit-startup-screen t)

(global-hl-line-mode)
