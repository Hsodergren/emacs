;; themes
(defun disable-all-themes ()
  (mapc 'disable-theme custom-enabled-themes))

(defun enable-themes (themes &optional only)
  (when only
	(disable-all-themes))
  (mapc 'load-theme themes))

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
