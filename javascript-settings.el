;; Due to mode-name aliasing Auto-complete cannot find js-mode. Hence make a symlink
(shell-command "ln -s ~/.emacs.d/packages/auto-complete/ac-dict/javascript-mode ~/.emacs.d/packages/auto-complete/ac-dict/js-mode")

(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))

;; CoffeeScript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(provide 'javascript-settings)
