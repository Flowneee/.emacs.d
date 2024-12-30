;; TODO AFTER FRESH INSTALL:
;;
;; * run M-x all-the-icons-install-fonts

(setq inhibit-startup-screen t)

;; Setup file for saving custom settings
(setq custom-file (expand-file-name "custom.el" (file-name-directory user-init-file)))
(load custom-file)

;; Initialize package.el and add MELPA to list of repos
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-native-compile t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed (since Emacs 29 use-package is built-in)
;; and configure it
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t
      use-package-expand-minimally t)

;; Automatically update ELPA keys
(use-package gnu-elpa-keyring-update)

;; Configure :delight (mode name hiding or customization) for use-package
(use-package delight)

;; Setup some UI global modes
(column-number-mode t) ;; Show line and column in mode line
(line-number-mode t)
(global-display-line-numbers-mode t) ;; Show line number left from content
(size-indication-mode t) ;; Show percentage in mode line
(blink-cursor-mode -1)
(global-hl-line-mode +1) ;; Highlight current line
(menu-bar-mode -1) ;; Remove bars on top of window
(tool-bar-mode -1)
(setq use-short-answers t) ;; Allow y/n instead of yes/no in prompts


;; Highlight text added by some operation
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

;; Set theme
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Set font
(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :foundry "PfEd"
                    :slant 'normal
                    :weight 'normal
                    :height 120
                    :width 'normal)
;; Set selection highlight
(set-face-attribute 'region nil :background "gray36")
(set-face-attribute 'highlight nil :background "dim gray")

;; Add support for nice icons in graphics mode
(use-package all-the-icons
  :if (display-graphic-p))

(use-package svg-lib
  :if (display-graphic-p))

;; Configure completion with Helm
(use-package helm
  :demand t ;; ensure helm is loaded immediately even with triggers present
  :custom
  ((helm-mode-fuzzy-match t)
   (helm-completion-in-region-fuzzy-match t)
   (helm-M-x-fuzzy-match t)
   (helm-buffers-fuzzy-matching t)
   (helm-recentf-fuzzy-match t)
   (helm-split-window-inside-p t)) ;; Ensure helm uses split when windows vertically split
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("M-TAB" . helm-completion-at-point))
  :config
  (helm-mode 1)) ;; Enable Helm globally

;; Configure company for autocompletion
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t))

;; Prompt for confirmation when quitting Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Move between windows with keyboard (windmove is builtin)
(global-set-key (kbd "S-<left>")  'windmove-left)
(global-set-key (kbd "S-<right>") 'windmove-right)  
(global-set-key (kbd "S-<up>")    'windmove-up)  
(global-set-key (kbd "S-<down>")  'windmove-down)

;; Move windows with Ctrl + Shift + <arrow>
(use-package buffer-move
  :ensure t
  :bind (("C-S-<down>"   . buf-move-down)   
         ("C-S-<up>"     . buf-move-up)     
         ("C-S-<left>"   . buf-move-left)   
         ("C-S-<right>"  . buf-move-right)))

(defun recompile-package (package)
  "Recompile a specific PACKAGE."
  (interactive "sEnter package name: ") 
  (let ((package-dir (or (locate-library package) 
                         (error "Package not found")))) 
    (byte-recompile-directory (file-name-directory package-dir) 0)
    (message "Package %s recompiled." package)))

;; Configure sidebar
(use-package neotree
  :bind
  ([f8] . neotree-toggle)
  :custom
  ((neo-theme (if (display-graphic-p) 'icons 'arrow))
   (neo-window-fixed-size nil)
   (neo-window-width 35)
   (neo-window-fixed-size t)
   (neo-show-hidden-files t))
  :hook
  (emacs-startup . neotree-show))

;; Enable automatic parenthesis insert
(use-package smartparens
  :hook (prog-mode text-mode markdown-mode)
  :bind
  (("M-<left>" . sp-forward-barf-sexp)
   ("M-<right>" . sp-forward-slurp-sexp))
  :config
  (require 'smartparens-config))

;; Setup some additional custom actions
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("S-<return>" . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above)))

(use-package move-text
  :bind
  (("C-M-<up>" . move-text-up)
   ("C-M-<down>" . move-text-down)))

(delete-selection-mode 1) ;; Delete selected text on typing
(setq-default indent-tabs-mode nil) ;; don't use tabs to indent
(setq-default tab-width 8) ;; but maintain correct appearance
(setq require-final-newline t) ;; Newline at end of file

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(defun include-file (file-name)
  "Include (load) file from relative to init.el location, appending .el"
  (load (expand-file-name (concat user-emacs-directory file-name))))

;; show the name of the current function definition in the modeline
(use-package which-func
  :config
  :delight
  (which-function-mode 1))

(use-package which-key
  :delight
  :custom
  (which-key-idle-delay 0.5)
  (which-key-popup-type 'minibuffer)
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; Highlight TODO NOTE FIXME
(use-package hl-todo
  :config
  (hl-todo-mode 1))

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

;; Configure Org mode (in org.el)
(include-file "org")

;; Configure generic LSP
(include-file "lsp")

;; ;; Configure Rust mode (in rust.el)
(include-file "rust")

