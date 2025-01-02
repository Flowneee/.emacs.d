;; Configure company for autocompletion
(use-package company
  :demand t
  :delight
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  :config
  (global-company-mode))

;; TODO: read documentation for Vertico/Marginalia/Orderless/Consult and improve configuration
;; TODO: add Embark

;; Vertical completion in separate buffer
(use-package vertico
  :demand t
  :init (vertico-mode)
  :custom (vertico-cycle t)
  :config
  (setq completion-in-region-function ;; Use vertico in for completion-in-region (M-TAB)
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Show additional information (e.g., file size, git status) in the minibuffer
(use-package marginalia
  :after vertico
  :init
  (marginalia-mode))

;; Fuzzy-matching in completion buffers
(use-package orderless
  :after vertico company
  :custom
  ((completion-styles '(orderless basic))  ;; Enable fuzzy matching with Orderless
   (completion-category-defaults nil)
   (completion-category-overrides '((file (styles partial-completion))))))  ;; Partial completion for file names

;; TODO: configure narrowing, search and other stuff
;; Advanced search and complete commands
(use-package consult
  :after vertico
  :bind
  (("M-y" . consult-yank-pop) ;; Yank (kill-ring)
   ("C-x b" . consult-buffer) ;; Buffer switching
   ("M-g g" . consult-goto-line)
   ("C-S-s" . consult-line))) ;; Go to line

