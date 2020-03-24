;;;(require 'whitespace)


;; (add-hook 'rust-mode-hook
;;           (lambda ()
;;             (setq-local whitespace-line-column 100)))
;; (require 'rust-mode)
;; (require 'company)

;; (require 'racer)
;; (add-hook 'rust-mode-hook #'racer-mode)
;; (add-hook 'racer-mode-hook #'eldoc-mode)

;; (add-hook 'racer-mode-hook #'company-mode)


;; ;;; RUST LANGUAGE SERVER SETTINGS
;; (require 'lsp-mode)
;; (use-package lsp-mode)
;; (require 'lsp-rust)
;; (add-hook 'rust-mode-hook #'lsp-rust-enable)

;; (require 'lsp-ui)
;; (use-package lsp-ui)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;; (add-hook 'rust-mode-hook 'flycheck-mode)

;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

;; (require' use-package)
;; (use-package rustic)

;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
;; (setq lsp-ui-doc-enable nil)
