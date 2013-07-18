;; Due to mode-name aliasing Auto-complete cannot find js-mode. Hence make a symlink
(shell-command "ln -s ~/.emacs.d/packages/auto-complete/dict/javascript-mode ~/.emacs.d/packages/auto-complete/dict/js-mode")

;; Node
(require 'js-comint)
(setq inferior-js-program-command "node") ;; not "node-repl"
(setenv "NODE_NO_READLINE" "1")

(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            (ansi-color-for-comint-mode-on)
            ;; Activate the folding mode
            (hs-minor-mode t)
            (local-set-key "\C-x\C-e" 'js-send-last-sexp)
            (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
            (local-set-key "\C-cb" 'js-send-buffer)
            (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
            (local-set-key "\C-cl" 'js-load-file-and-go)))

;; CoffeeScript mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))

(provide 'javascript-settings)
