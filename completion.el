;; NOTE: if fuzzy-matching not working, most probable cause is orderless not being loaded correctly

;; Ignore case for when selecting file or buffer to open
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

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
  :custom (vertico-cycle t)
  :config
  (vertico-mode)
  (vertico-mouse-mode)  ;; Support mouse in vertico buffers
  (setq completion-in-region-function ;; Use vertico in for completion-in-region (M-TAB)
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Show additional information (e.g., file size, git status) in the minibuffer
(use-package marginalia
  :after vertico
  :custom
  ((marginalia-align 'left))
  :config
  (marginalia-mode))

;; Add icons to marginalia hints
(use-package nerd-icons-completion
  :after marginalia nerd-icons
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

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
  :custom
  ((xref-show-xrefs-function #'consult-xref) ;; Replace xref default UI with consult
   (xref-show-definitions-function #'consult-xref))
  :bind
  (("M-y" . consult-yank-pop) ;; Yank (kill-ring)
   ("C-x b" . consult-buffer) ;; Buffer switching
   ("M-g g" . consult-goto-line) ;; Go to line
   ("C-s" . consult-line) ;; Find in buffer
   ("C-S-s" . isearch-forward))) ;; Remap default forward search
