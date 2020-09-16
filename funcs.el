
;; themes
(defun disable-all-themes ()
  (mapc 'disable-theme custom-enabled-themes))

(defun enable-themes (themes &optional only)
  (when only
	(disable-all-themes))
  (mapc 'enable-theme themes))
