;; NOTE: flycheck is enabled in init.el

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . flycheck-mode)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols))
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
  ((lsp-ui-doc-enable t)
   (lsp-ui-sideline-enable t)
   (lsp-ui-sideline-show-code-actions t)
   (lsp-ui-sideline-show-diagnostics t)
   (lsp-ui-sideline-show-hover t)
   (lsp-ui-sideline-wait-for-all-symbols nil)
   (lsp-ui-peek-enable t)))

(use-package consult-lsp
  :after consult lsp-mode
  :commands consult-lsp-symbols)

;; TODO: configure DAP mode
;; for now it mostly broken:
;;   * dap-gdb-lldb always ends with 'spawn gdn ENOENT'
;;   * dap-gdb works, but no UI appear
;; or maybe try dape

;; (use-package dap-mode
;;   :commands dap-mode
;;   :config
;;   (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-ui-controls-mode 1)

;;   (require 'dap-gdb)
  ;; (require 'dap-lldb)
  ;; (require 'dap-gdb-lldb)
  ;; (dap-gdb-lldb-setup)

  ;; (dap-register-debug-template "Rust::GDB Run Configuration"
  ;;                            (list :type "gdb"
  ;;                                  :request "launch"
  ;;                                  :name "GDB::Run"
  ;;                          :gdbpath "rust-gdb"
  ;;                                  :target nil
  ;;                                  :cwd nil))
  ;; )
