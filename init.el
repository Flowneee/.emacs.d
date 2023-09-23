;;;

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 )

(require 'yasnippet)
(yas-global-mode 1)
(toggle-scroll-bar nil)
(tool-bar-mode -1)
(menu-bar-mode 1)
(setq prelude-flyspell nil)
;;(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-c ;") 'iedit-mode)
;; (global-linum-mode t) replaced with display-line-mumber
(setq confirm-kill-emacs 'yes-or-no-p)
(guru-mode 1)
;; (nyan-mode 0)
(global-whitespace-mode 0)
(require 'editorconfig)
(editorconfig-mode 1)
(setq ffap-machine-p-known 'reject)

;; TODO: remove or not
(setq rust-match-angle-brackets nil)
(setq bidi-inhibit-bpa t)
(setq bidi-paragraph-direction "left-to-right")

(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-completion-provider :capf)

(defvar path_prefix "~/.emacs.d/personal/")

(defun my:local_load (file)
  (load (concat path_prefix file)))

;; Remove ugly strike-through from Rust warnings
(setq lsp-diagnostics-attributes
  `((unnecessary)
    (deprecated) )
  )

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
(my:local_load "./settings/sql.el")
(my:local_load "./settings/multiple-cursor.el")

(require 'projectile-speedbar)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(add-hook 'text-mode-hook 'highlight-indent-guides-mode)

(add-to-list 'auto-mode-alist '("Jenkinsfile(-[a-zA-Z0-9_\\-]+)?\\'" . jenkinsfile-mode))

;;; .init.el ends here
