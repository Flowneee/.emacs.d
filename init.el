;; Change temporary directory to have emacs/ directory inside it and use new path to store
;; all temporary data Emacs needed.
(setq temporary-file-directory
      (let ((dir (expand-file-name "emacs" temporary-file-directory)))
        (unless (file-exists-p dir)
          (make-directory dir t))
        dir))

;; Setup file for saving custom settings
(setq custom-file (expand-file-name "custom.el" (file-name-directory user-init-file)))
(unless (file-exists-p custom-file)
  (with-temp-buffer
    (write-file custom-file)))
(load custom-file)

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Allow execute M-x inside minibuffer
(setq enable-recursive-minibuffers t)

;; Initialize package.el and add MELPA to list of repos
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(setq package-native-compile t
      package-install-upgrade-built-in t) ;; Allow to upgrade built-in packages from repo
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

;; Load environment variables into Emacs
(use-package exec-path-from-shell
  :demand t
  :config (exec-path-from-shell-initialize))

;; Configure :delight (mode name hiding or customization) for use-package
(use-package delight)

;; Hide some built-in modes, use use-package to automatically ensure modes are loaded
(use-package subword
  :ensure nil
  :delight)
(use-package eldoc
  :ensure nil
  :delight)

(defun include-file (file-name)
  "Include and execute FILE-NAME.el from relative to init.el location."
  (load (expand-file-name (concat user-emacs-directory file-name))))

(defun recompile-package (package)
  "Recompile a specific PACKAGE."
  (interactive "sEnter package name: ")
  (let ((package-dir (or (locate-library package)
                         (error "Package not found"))))
    (byte-recompile-directory (file-name-directory package-dir) 0)
    (message "Package %s recompiled." package)))

;; Prepare tree-sitter shared libs for built-in tree-sitter support
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
  ;; If used with Emacs 29 or newer, copy downloaded libraries to location expected by
  ;; built-in tree-sitter support.
  (when (version<= "29.0" emacs-version)
    (setup-tree-sitter-libs)))

;; Disable Ispell integration with debian
(remove-hook 'after-init-hook 'debian-ispell-set-default-dictionary)

(include-file "editor")
(include-file "ui")
(include-file "completion")

;; Configure syntax checking
(use-package flycheck
  :config
  (global-flycheck-mode))

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
  :after crux
  :custom
  (jsonian-indentation 4)
  :config
  (crux-with-region-or-buffer jsonian-format-region))

(use-package yaml-pro
  :mode ("\\.ya?ml\\'" . yaml-ts-mode)
  :hook ((yaml-ts-mode . subword-mode)
         (yaml-ts-mode . yaml-pro-ts-mode)))

(use-package nxml-mode
  :ensure nil ;; Built-in package, for some reason breaks on :ensure t
  :mode "\\.xml\\'"  ;; Detect XML files based on the extension as well as XML prolog
  :magic "<\\?xml"
  :custom
  ((nxml-child-indent 4)
   (nxml-attribute-indent 4)
   (nxml-auto-insert-xml-declaration-flag nil)
   (nxml-bind-meta-tab-to-complete-flag t)
   (nxml-slash-auto-complete-flag t)))

