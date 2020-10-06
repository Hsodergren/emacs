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
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "dcea7310d9d7c169f6343663ff427c3249ef84c21d0179a140183c28378e96ca" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"))
 '(dired-dwim-target t)
 '(dired-use-ls-dired t)
 '(disable-mouse-global-mode t nil (disable-mouse))
 '(eldoc-idle-delay 0.2)
 '(flycheck-display-errors-delay 0.4)
 '(frame-brackground-mode 'dark)
 '(helm-M-x-reverse-history nil)
 '(helm-allow-mouse t)
 '(helm-completion-style 'emacs)
 '(inhibit-startup-screen t)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(org-agenda-files '("~/org/"))
 '(org-fontify-done-headline t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-package-always-ensure t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :foundry "ADBO" :family "Source Code Pro"))))
 '(cursor ((((background light)) (:background "ForestGreen"))))
 '(linum ((t (:foreground "chocolate"))))
 '(merlin-type-face ((((background dark)) (:inherit caml-types-expr-face :background "#333")) (((background light)) (:inherit caml-types-expr-face :background "#cff"))))
 '(region ((((background light)) (:extend t :background "beige" :distant-foreground "black"))))
 '(show-paren-mismatch ((t (:inherit caml-types-expr-face :background "#f33" :foreground "white")))))
