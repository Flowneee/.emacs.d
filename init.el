;;;

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 )

(require 'yasnippet)
(toggle-scroll-bar nil)
(tool-bar-mode -1)
(menu-bar-mode 1)
(setq prelude-flyspell nil)
;; (global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c ;") 'iedit-mode)
(global-linum-mode t)
(setq confirm-kill-emacs 'yes-or-no-p)
(guru-mode 1)
;; (nyan-mode 0)
(global-whitespace-mode 0)

(defvar path_prefix "~/.emacs.d/personal/")

(defun my:local_load (file)
  (load (concat path_prefix file)))

;; (my:local_load "./settings/custom-functions.el")
(my:local_load "./settings/speedbar.el")
(my:local_load "./settings/helm.el")
(my:local_load "./settings/purpose.el")
(my:local_load "./settings/ui.el")
(my:local_load "./settings/guru-mode.el")
(my:local_load "./settings/web-mode.el")
(my:local_load "./settings/hs-mode.el")
;; (my:local_load "./settings/elpy.el")
(my:local_load "./settings/rust.el")
(my:local_load "./settings/rich-minority.el")
(require 'projectile-speedbar)

;;; .init.el ends here
