;; This should enable builtin treesiter, but it more ugly than external and also broken)))
;; https://github.com/rust-lang/rust-mode/issues/524
;;(setq rust-mode-treesitter-derive t)

(use-package rustic
  :defer t
  :commands rustic-mode
  :hook ((rustic-mode . lsp-deferred)
         (rustic-mode . cargo-minor-mode)
         (rustic-mode . subword-mode))
  :custom ((rustic-lsp-client 'lsp-mode)
           (lsp-rust-analyzer-library-directories
            '("~/.cargo/registry/src" "~/.rustup/toolchains" "~/.cargo/git/checkouts")))
  :config
  (setq rustic-format-on-save t)) ;; For some reason doesn't work in :custom section

(use-package cargo
  :delight
  :commands cargo-minor-mode)

;; TODO: check on later versions of Emacs if built-in treesit becomes better with Rust
;;       OR just fix color scheme myself
(use-package tree-sitter
  :hook rustic-mode
  :delight
  :config
  (require 'tree-sitter-langs)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
