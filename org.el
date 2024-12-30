(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :hook (org-mode . org-indent-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :custom
  (org-list-indent-offset 2)
  (indent-tabs-mode nil)
  (org-log-done t)
  (org-log-into-drawer t))

