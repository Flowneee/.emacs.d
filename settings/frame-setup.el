;;; Frame setup

(defun my:frame-setup (sidebar)
  (delete-other-windows)
  (if (equal sidebar "neotree")
      (progn
	(require 'neotree)
	(neotree-toggle)
	(neotree-hidden-file-toggle)))
  (if (equal sidebar "sr-speedbar")
      (my:sr-speedbar-open))
  (windmove-right)
  ;(setq left-top-window (my:get-current-window))
  (if (equal (display-graphic-p) t)
      (progn
	(split-window-below)
	(split-window-right)
	(windmove-right)
	;(setq right-top-window (my:get-current-window))
	(windmove-down)
	(setq bottom-window (my:get-current-window))
	(eshell)
	(my:toggle-window-dedicated)
	(minimize-window bottom-window)
	(window-resize bottom-window 7)
	(linum-mode -1)
	(other-window -2))))
(add-hook 'window-setup-hook '(lambda () (my:frame-setup "sr-speedbar")))

;;; frame-setup.el ends here
