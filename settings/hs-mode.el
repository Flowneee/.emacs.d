;;;

(add-hook 'prog-mode-hook #'hs-minor-mode)

(global-set-key (kbd "C-c <up>") 'hs-hide-block)
(global-set-key (kbd "C-c <down>") 'hs-show-block)
;;;
