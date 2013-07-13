(setq py-install-directory "~/.emacs.d/packages/python-mode/")
(require 'python-mode)
(require 'python-pep8)
(require 'python-pylint)

;; Use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

;; Switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; Don't split windows
(setq py-split-windows-on-execute-p t)
;; Try to automagically figure out indentation
(setq py-smart-indentation t)

(add-hook 'python-mode-hook 'smart-operator-mode)
(provide 'python-settings)
