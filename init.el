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

(defun include-file (file-name)
  "Include and execute file from relative to init.el location, appending .el"
  (load (expand-file-name (concat user-emacs-directory file-name))))

(defun recompile-package (package)
  "Recompile a specific PACKAGE."
  (interactive "sEnter package name: ") 
  (let ((package-dir (or (locate-library package) 
                         (error "Package not found")))) 
    (byte-recompile-directory (file-name-directory package-dir) 0)
    (message "Package %s recompiled." package)))

;; Prepare tree-sitter shared libs
;; On Arch Linux some libs are also available in repos, but it is easier to install them
;; all in one step regardless of distro.
(defun setup-tree-sitter-libs ()
  "Set up tree-sitter libraries by copying .so files to the designated directory."
  (unless (bound-and-true-p updated-tree-sitter-libs)
    (let* ((init-dir (file-name-as-directory
                      (expand-file-name user-emacs-directory))) ;; Use actual init directory
           (package-install-dir (file-name-as-directory
                                 (expand-file-name "elpa" init-dir))) ;; Find package location
           (tree-sitter-dir (directory-files package-install-dir t
                                             "^tree-sitter-langs-[0-9.]+"))
           (dest-dir (file-name-as-directory
                      (expand-file-name "tree-sitter" init-dir)))) ;; Build destination directory path
      (when tree-sitter-dir
        ;; Ensure the destination directory exists
        (unless (file-directory-p dest-dir)
          (make-directory dest-dir t))
        ;; Copy .so files from <tree-sitter-dir>/bin to <init-dir>/tree-sitter
        (let ((bin-dir (expand-file-name "bin" (car tree-sitter-dir))))
          (when (file-directory-p bin-dir)
            (dolist (file (directory-files bin-dir nil "\\.so\\'"))
              (let* ((source-file (expand-file-name file bin-dir))
                     (base-name (file-name-base file)) ;; Extract base name without extension
                     (dest-file (expand-file-name (format "libtree-sitter-%s.so" base-name) dest-dir)))
                (copy-file source-file dest-file t))))))
      ;; Set the variable and save it to the custom file
      (customize-save-variable 'updated-tree-sitter-libs t))))

(use-package tree-sitter-langs
  :delight
  :config
  (setup-tree-sitter-libs))

(include-file "editor")
(include-file "ui")

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

;; File-specific modes (programming manguages, configs, etc)

(include-file "org")

;; LSP and languages with LSP support
(include-file "lsp")
(include-file "rust")

;; Configuration files
(use-package toml-mode
  :mode "\\.toml\\'"
  :hook (toml-mode . subword-mode))

(use-package jsonian
  :mode ("\\.jsonc?\\'" . jsonian-mode)
  :hook (jsonian-mode . subword-mode)
  :custom
  (jsonian-indentation 4)
  :config
  (crux-with-region-or-buffer jsonian-format-region))

(use-package yaml-pro
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook ((yaml-ts-mode . subword-mode)
         (yaml-ts-mode . yaml-pro-ts-mode)))
