(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f"
    "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "8dcc1b5030f5da3326c9df093c4a6ed006221ffa093e3b328f5037abc05d2b65"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "dcea7310d9d7c169f6343663ff427c3249ef84c21d0179a140183c28378e96ca"
     "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"))
 '(dired-dwim-target t)
 '(dired-use-ls-dired t)
 '(disable-mouse-global-mode t nil (disable-mouse))
 '(eldoc-idle-delay 0.2)
 '(evil-collection-mode-list
   '(magit xref dired wdired custom eldoc package-menu compile image pdf
           embark ediff consult))
 '(evil-undo-system 'undo-redo)
 '(flycheck-display-errors-delay 0.4)
 '(frame-brackground-mode 'dark)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(key-chord-mode t)
 '(key-chord-safety-interval-forward 0.0)
 '(load-prefer-newer t)
 '(menu-bar-mode nil)
 '(notmuch-hello-refresh-hook '(my/receive-mail))
 '(notmuch-search-oldest-first nil)
 '(org-agenda-files '("~/org/"))
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(org-fontify-done-headline t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(send-mail-function 'mailclient-send-it)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(use-package-always-ensure t)
 '(warning-suppress-log-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 100 :foundry "ADBO" :family "Source Code Pro"))))
 '(cursor ((((background light)) (:background "ForestGreen"))))
 '(flymake-error ((t (:underline (:color "#cd0000" :style wave :position wave)))))
 '(flymake-note ((t (:inherit warning))))
 '(flymake-warning ((t (:underline (:color "#8b5a00" :style wave :position wave)))))
 '(lsp-details-face ((t (:inherit shadow :height 0.8))))
 '(merlin-type-face ((((background dark)) (:inherit caml-types-expr-face :background "#333")) (((background light)) (:inherit caml-types-expr-face :background "#cff"))))
 '(region ((((background light)) (:extend t :background "beige" :distant-foreground "black"))))
 '(shadow ((t (:foreground "#92898e"))))
 '(show-paren-mismatch ((t (:inherit caml-types-expr-face :background "#f33" :foreground "white")))))
