(autoload 'python-pep8 "python-pep8")
(autoload 'python-pylint "python-pylint")
(autoload 'nose "nose")
(autoload 'deferred "deferred")
(autoload 'epc "epc")

(setq jedi:setup-keys t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

(provide 'python-settings)
