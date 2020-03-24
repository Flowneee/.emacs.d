;;; ui.el --- Emacs UI vustomization.
(require 'framemove)
;;(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)
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
;;;(setq sml/no-confirm-load-theme nil)
;;;(remove-hook 'after-init-hook #'sml/setup)

(defun my:set-window-purpose-dedicated (flag)
  "Enable (flag is t) or disable (flag is nil) purpose dedicated of selected window"
  (if (not (equal (purpose-window-purpose-dedicated-p) flag))
      (purpose-toggle-window-purpose-dedicated))
  (force-mode-line-update)
  flag)

(defun my:open-sidebar (sidebar &optional enable-purpose-dedicated)
  "Arguments is string 'sr-speedbar' or 'neotree'."
  (if (equal sidebar "neotree")
      (progn
	(require 'neotree)
	(neotree-toggle)
	(neotree-hidden-file-toggle)))
  (if (equal sidebar "sr-speedbar")
      (my:sr-speedbar-open))
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (windmove-right))

(defun my:split-with-bottom-term (terminal &optional enable-purpose-dedicated)
  "Split window vertically to 2 edit windows and place TERMINAL to bottom"
  (split-window-below)
  (split-window-right)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (windmove-right)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (windmove-down)
  ;;; Setting terminal
  (setq bottom-window (my:get-current-window))
  (funcall terminal)
  (my:toggle-window-dedicated)
  (minimize-window bottom-window)
  (window-resize bottom-window 7)
  (linum-mode -1)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (set-window-dedicated-p bottom-window t)
  (other-window -2))

;;; Interactive wrapper to previous function
(defun split-with-bottom-term ()
  "Split window vertically to 2 edit windows and place multi-term to bottom"
  (interactive)
  (my:split-with-bottom-term 'multi-term t))

(defun my:split-with-right-term (terminal &optional enable-purpose-dedicated)
  "Split window vertically to edit window and in right splitted horizontally
edit window and TERMINAL."
  (split-window-right)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (setq this-window (my:get-current-window))
  (window-resize this-window 7 t)
  (windmove-right)
  ;;; Set side
  (split-window-below)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (windmove-down)
  (funcall terminal)
  (my:set-window-purpose-dedicated enable-purpose-dedicated)
  (my:toggle-window-dedicated)
  (linum-mode -1)
  (other-window -2))

;;; Interactive wrapper to previous function
(defun split-with-right-term ()
  "Split window vertically to edit window and in right splitted horizontally
edit window and multy-term."
  (interactive)
  (my:split-with-right-term 'multi-term t))

(defun my:old-frame-setup (sidebar &optional set-purpose-dedicated)
  (delete-other-windows)
  ;;; Set Dir browser
  ;;;(if (equal sidebar "neotree")
  ;;;    (progn
  ;;;      (require 'neotree)
  ;;;      (neotree-toggle)
  ;;;      (neotree-hidden-file-toggle)))
  ;;;(if (equal sidebar "sr-speedbar")
  ;;;    (my:sr-speedbar-open))
  ;;;(windmove-right)
  (my:open-sidebar "sr-speedbar" t)
  ;;;(setq left-top-window (my:get-current-window))

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
;;;(add-hook 'window-setup-hook '(lambda () (my:frame-setup "sr-speedbar")))
;;;cuz instead use purpose-mode
(defun my:new-frame-setup ()
  (delete-other-windows)
  (my:open-sidebar "sr-speedbar" t))
(add-hook 'window-setup-hook '(lambda () (my:new-frame-setup)))


(global-set-key (kbd "<C-S-down>")   'buf-move-do)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

;;; ui.el ends here
