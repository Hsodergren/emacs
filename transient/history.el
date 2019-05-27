((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-commit nil
	       ("--reuse-message=master"))
 (magit-diff
  ("--no-ext-diff" "--stat"))
 (magit-dispatch nil)
 (magit-gitignore nil)
 (magit-log
  ("-n256" "--graph" "--decorate")
  nil
  ("-n256" "--graph" "--color" "--decorate")
  ("-n256" "-Ghistorysml" "--graph" "--decorate")
  ("-n256" "-Ghistory" "--graph" "--color" "--decorate")
  (("--" "init.el"))
  ("--author=Henrik Södergren <henrik.sodergren@outlook.com>"
   ("--" "init.el")
   "--graph" "--color" "--decorate")
  (("--" "init.el")
   "--graph" "--color" "--decorate")
  (("--" "init.el")
   "--graph" "--color" "++header")
  (("--" "init.el")
   "--graph"))
 (magit-log:-G "historysml" "history")
 (magit-pull
  ("--rebase")
  nil)
 (magit-push nil)
 (magit-revision-history "master")
 (magit:--author "Henrik Södergren <henrik.sodergren@outlook.com>"))
