;; Speedbar and sr-speedbar settings

(require 'speedbar)
(require 'sr-speedbar)
(setq speedbar-show-unknown-files t)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 30)
(setq speedbar-directory-unshown-regexp "^\\(CVS\\|RCS\\|SCCS\\|\\.\\.*$\\)\\'")

(unless (display-graphic-p)
  (setq speedbar-use-images nil))


(defun my:sr-speedbar-open ()
  (sr-speedbar-open)
  (with-current-buffer sr-speedbar-buffer-name
    (setq window-size-fixed 'width)))

(defun my:toggle-sr-speedbar-refresh ()
  "Control whether or not Sr-Speedbar is allowed to refresh content"
  (interactive)
  (if (equal sr-speedbar-auto-refresh nil)
      (sr-speedbar-refresh-turn-on)
    (sr-speedbar-refresh-turn-off)))
(global-set-key (kbd "C-S-r") 'my:toggle-sr-speedbar-refresh)
