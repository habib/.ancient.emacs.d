;; Clojure mode
(require 'clojure-mode)
(eval-after-load 'clojure-mode
 `(progn
    (subword-mode +1)))

(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

(eval-after-load 'nrepl
  '(progn
     (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
     (subword-mode +1)))

(provide 'clojure-settings)
