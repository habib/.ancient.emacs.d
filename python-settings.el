(setq py-install-directory "~/.emacs.d/packages/python-mode/")

(setq py-load-pymacs-p nil)
(require 'python-mode)
(require 'python-pep8)
(require 'python-pylint)

(require 'nose)

;; Use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

;(setq ac-sources '(ac-source-pycomplete))

;; Switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
;; Don't split windows
(setq py-split-windows-on-execute-p t)
;; Try to automagically figure out indentation
(setq py-smart-indentation t)

(add-hook 'python-mode-hook 'smart-operator-mode)
(provide 'python-settings)
