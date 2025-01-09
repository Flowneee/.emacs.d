;; Editing-related stuff (also navigation)

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

(blink-cursor-mode -1)
(global-hl-line-mode +1) ;; Highlight current line

;; Enable some "dangerous" commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Remap some commands
(keymap-global-set "<backspace>" 'backward-delete-char-untabify)
(keymap-global-set "<delete>" 'delete-forward-char)
(keymap-global-set "M-<backspace>" 'backward-kill-word)
(keymap-global-set "M-<delete>" 'kill-word)

;; Highlight text added by some operation
(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

;; Enable automatic parenthesis insert
(use-package smartparens
  :delight
  :hook (prog-mode text-mode markdown-mode)
  :bind (("M-S-<left>" . sp-forward-barf-sexp)
         ("M-S-<right>" . sp-forward-slurp-sexp))
  :config
  (require 'smartparens-config))

;; Setup some additional custom actions
(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line)
   ("S-<return>" . crux-smart-open-line)
   ("C-S-<return>" . crux-smart-open-line-above))
  :config
  (crux-with-region-or-line kill-region) ;; Make C-w operate on line if no region selected
  )

(use-package move-text
  :bind
  (("M-S-<up>" . move-text-up)
   ("M-S-<down>" . move-text-down)))

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

;; Highlight TODO NOTE FIXME
(use-package hl-todo
  :demand t ;; for some reason doesn't work without demand
  :delight
  :config
  (global-hl-todo-mode t))

;; Enable .editorconfig support
(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

;; Move between windows with keyboard
(use-package windmove
  :bind (("S-<left>" . windmove-left)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("S-<down>" . windmove-down)))

;; Move windows with Ctrl + Shift + <arrow>
(use-package buffer-move
  :ensure t
  :bind (("C-S-<down>"   . buf-move-down)
         ("C-S-<up>"     . buf-move-up)
         ("C-S-<left>"   . buf-move-left)   
         ("C-S-<right>"  . buf-move-right)))

;; Highlight indentations in programming modes
(use-package highlight-indent-guides
  :hook prog-mode
  :custom ((highlight-indent-guides-method 'character)
           (highlight-indent-guides-responsive 'nil)
           (highlight-indent-guides-auto-character-face-perc 40)))

;; Hide code blocks
(use-package hideshow
  :ensure nil ;; hideshow is built-in
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-x <up>" . hs-hide-block)
         ("C-x <down>" . hs-show-block)
         ("C-x S-<up>" . hs-hide-level))
  :custom ((hs-isearch-open t)
           (hs-hide-comments-when-hiding-all t)))

;; TODO: adjust keybindings
(use-package undo-tree
  :config
  (global-undo-tree-mode)
  :bind ("C-x u" . undo-tree-visualize))
