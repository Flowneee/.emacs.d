;;; ui.el --- Emacs UI vustomization.

;;; Code:

(defun my:toggle-window-dedicated ()
  "Control whether or not Emacs is allowed to display another
buffer in current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "%s: Can't touch this!"
     "%s is up for grabs.")
   (current-buffer)))
(global-set-key (kbd "C-S-d") 'my:toggle-window-dedicated)

(defun my:get-current-window ()
  "Get current active window"
  (get-buffer-window (current-buffer)))

(beacon-mode -1)
;(setq sml/no-confirm-load-theme nil)
;(remove-hook 'after-init-hook #'sml/setup)

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

;;; ui.el ends here
