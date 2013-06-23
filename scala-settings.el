(require 'scala-mode2)
(add-hook 'scala-mode-hook
          '(lambda ()
             (autopair-mode)
             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(add-to-list 'load-path "~/ensime/src/main/elisp")
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'scala-settings)
