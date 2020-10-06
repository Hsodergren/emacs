;; themes
(defun disable-all-themes ()
  (mapc 'disable-theme custom-enabled-themes))

(defun enable-themes (themes &optional only)
  (when only
	(disable-all-themes))
  (mapc 'load-theme themes))

(defun dark-theme ()
  (interactive)
  (enable-themes (list 'gruber-darker 'smart-mode-line-dark) t))

(defun light-theme ()
  (interactive)
  (disable-all-themes))

(defun time-sensitive-theme ()
  (interactive)
  (let ((hour (caddr (decode-time (current-time)))))
	(if (< 9 hour 17)
		(light-theme)
	  (dark-theme))))

;; line
(defvar my-end-screen-buffer 5)

(defun my-next-line ()
  (interactive)
  (let* ((top (line-number-at-pos (window-start)))
		 (bottom (+ (window-height) top))
		 (current (line-number-at-pos)))
	(if (<= (- bottom current) my-end-screen-buffer)
		(progn
		  (evil-next-line)
		  (evil-scroll-line-down 1))
	  (evil-next-line))))

(defun my-prev-line ()
  (interactive)
  (let ((top (line-number-at-pos (window-start)))
		(bottom (line-number-at-pos (window-end)))
		(current (line-number-at-pos)))
	(if (<= (- current top) my-end-screen-buffer)
		(progn
		  (evil-previous-line)
		  (evil-scroll-line-up 1))
	  (evil-previous-line))))
