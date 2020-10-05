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
   '("8dcc1b5030f5da3326c9df093c4a6ed006221ffa093e3b328f5037abc05d2b65" "37c72b2dd09c6d35befa8470f056a6b3a78837397819461fb2c0750f0dc8e5f2" "fb1d4e7187ffa854d38c32989d5a921a317972dc14692227e309a826ddb53e27" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5f824cddac6d892099a91c3f612fcf1b09bb6c322923d779216ab2094375c5ee" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1a212b23eb9a9bedde5ca8d8568b1e6351f6d6f989dd9e9de7fba8621e8ef82d" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "79485bab8bb220562d4acd003e4b6f1c9005af41e91f81b7a0e89b7e3a301203" "acfac6b14461a344f97fad30e2362c26a3fe56a9f095653832d8fc029cb9d05c" "e396098fd5bef4f0dd6cedd01ea48df1ecb0554d8be0d8a924fb1d926f02f90f" "33af2d5cb040182b798c3a4ee6d16210e700a2fabaa409231e1c4a003cafd1c2" "2540689fd0bc5d74c4682764ff6c94057ba8061a98be5dd21116bf7bf301acfb" "24fc62afe2e5f0609e436aa2427b396adf9a958a8fa660edbaab5fb13c08aae6" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" "49113d0c7d23c1f752409d0d4d34d3f5af4f1692f5dd6d1b1b3c805ca2d606aa" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default))
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
