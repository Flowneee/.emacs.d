;; NOTE: flycheck is enabled in init.el

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?)                             ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection))  ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . flycheck-mode)
  :init
  (setq lsp-keymap-prefix "C-c C-l")
  :bind (:map lsp-mode-map
              ([remap xref-find-apropos] . consult-lsp-symbols)
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :custom
  ((lsp-enable-snippet t)
   (lsp-log-io nil)
   (lsp-prefer-flymake nil)
   (lsp-lens-enable nil)
   (lsp-enable-symbol-highlighting t))
  :config
  ;; Integrate emacs-lsp-booster https://github.com/blahgeek/emacs-lsp-booster?tab=readme-ov-file#configure-lsp-mode
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

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
