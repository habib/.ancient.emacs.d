(require 'python-pep8)
(require 'python-pylint)
(require 'nose)
(require 'deferred)
(require 'epc)

(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'python-settings)
