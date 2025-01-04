;; UI-related stuff (modeline, sidebar, ...)

;; Set theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package display-line-numbers ;; Show line number left from content
  :hook prog-mode text-mode conf-mode)

;; Modeline modes
(column-number-mode t) ;; Show line and column in mode line
(line-number-mode t)
(size-indication-mode t) ;; Show percentage in mode line

;; Show the name of the current function definition in the modeline
(use-package which-func
  :config
  :delight
  (which-function-mode 1))

;; TODO: check after Emacs 30 because package will become built-in, some options might change
;; Display the key bindings following your currently entered incomplete command.
(use-package which-key
  :delight
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Frame UI modes
(menu-bar-mode -1) ;; Remove bars on top of window
(tool-bar-mode -1)
(setq use-short-answers t) ;; Allow y/n instead of yes/no in prompts
(setq confirm-kill-emacs 'y-or-n-p) ;; Prompt for confirmation when quitting Emacs
(add-hook 'kill-emacs-query-functions ;; Check unsaved customizations
          'custom-prompt-customize-unsaved-options)

;; TODO: figure out why icons are weird when Emacs in terminal or disable them with if display-graphic-p
;; Add support for Nerd icons in graphics mode
(use-package nerd-icons
  :config
  (unless (bound-and-true-p nerd-icons-installed)
    (nerd-icons-install-fonts)
    (customize-save-variable 'nerd-icons-installed t)))

(use-package svg-lib
  :if (display-graphic-p))

;; Configure sidebar
(use-package neotree
  :bind
  ([f8] . neotree-toggle)
  :custom
  ((neo-theme 'nerd-icons)
   (neo-window-fixed-size nil)
   (neo-window-width 35)
   (neo-window-fixed-size t)
   (neo-show-hidden-files t))
  :hook
  (emacs-startup . neotree-show))
