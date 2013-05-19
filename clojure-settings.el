;; Clojure mode
(require 'clojure-mode)
(eval-after-load 'clojure-mode
 `(progn
    (subword-mode +1)))

(require 'nrepl)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-to-list 'same-window-buffer-names "*nrepl*")
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces-in-repl t)
(setq nrepl-history-file "~/.emacs.d/nrepl-history")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)

(eval-after-load 'nrepl
  '(progn
     (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
     (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
     (nrepl-enable-on-existing-clojure-buffers)
     (subword-mode +1)))

;; Auto completion for NREPL
(require 'ac-nrepl)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

(provide 'clojure-settings)
