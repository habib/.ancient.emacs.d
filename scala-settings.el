(require 'scala-mode2)
(add-to-list 'load-path "~/ensime/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'scala-settings)
