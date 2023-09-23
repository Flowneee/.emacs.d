(require 'sql)

(require 'sqlup-mode)
(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)

;;(require 'sql-ident)
;; (add-hook 'sql-mode-hook 'sqlind-minor-mode)

