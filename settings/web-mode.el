;;;

(require 'web-mode)

(defun my:web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (web-mode-set-engine "django"))

(add-hook 'web-mode-hook  'my:web-mode-hook)
