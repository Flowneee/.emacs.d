(require 'rich-minority)
(rich-minority-mode 1)
(setq rm-blacklist nil)
(setq rm-whitelist
      (format "^ \\(%s\\)$"
              (mapconcat #'identity
                         '("LSP.*" "Blame")
                         "\\|")))
