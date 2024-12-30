(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :custom
  ((lsp-enable-snippet t)
   (lsp-log-io nil)
   (lsp-prefer-flymake nil)
   (lsp-lens-enable nil)
   (lsp-enable-symbol-highlighting t)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-sideline-show-hover t)            
  (lsp-ui-sideline-wait-for-all-symbols nil)
  (lsp-ui-peek-enable t))

(use-package helm-lsp
  :requires (lsp-mode helm)
  :commands helm-lsp-workspace-symbol
  :custom
  (helm-lsp-fuzzy-match t))

;; (use-package dap-mode
;;   :commands dap-mode
;;   :config
;;   (require 'dap-ui)
;;   (dap-ui-mode 1))

;; ;; Optional, improves LSP performance
;; (use-package flycheck
;;   :hook (lsp-mode . flycheck-mode))
