(use-package org
  :defer t
  :requires htmlize
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . org-indent-mode)
         (org-mode . org-custom-bindings-hook))
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :custom ((org-list-indent-offset 2)
           (indent-tabs-mode nil)
           (org-log-done t)
           (org-log-into-drawer t)
           (org-export-with-sub-superscripts '{}))
  :config
  ;; Define custom keymap for windmove and bufmove in Org mode
  (defvar custom-org-keymap (make-sparse-keymap)
    "Custom keymap for windmove and buffer move in Org mode.")
  
  (define-key custom-org-keymap (kbd "S-<left>") 'windmove-left)
  (define-key custom-org-keymap (kbd "S-<right>") 'windmove-right)
  (define-key custom-org-keymap (kbd "S-<up>") 'windmove-up)
  (define-key custom-org-keymap (kbd "S-<down>") 'windmove-down)

  (define-key custom-org-keymap (kbd "C-S-<left>") 'buf-move-left)
  (define-key custom-org-keymap (kbd "C-S-<right>") 'buf-move-right)
  (define-key custom-org-keymap (kbd "C-S-<up>") 'buf-move-up)
  (define-key custom-org-keymap (kbd "C-S-<down>") 'buf-move-down)

  (defun org-custom-bindings-hook ()
    "Set up custom bindings for Org mode."
    (define-key org-mode-map (kbd "C-x w") custom-org-keymap)))

(use-package htmlize
  :defer t)
