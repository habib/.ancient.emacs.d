(add-to-list 'load-path
             (concat user-emacs-directory
                     (file-name-as-directory "ensime/src/main/elisp")))

(autoload 'scala-mode2 "scala-mode2")
(add-hook 'scala-mode-hook
          '(lambda ()
             (autopair-mode)
             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(autoload 'ensime "ensime")
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'scala-settings)
