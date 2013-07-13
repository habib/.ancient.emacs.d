;; Our packages are placed under the packages folder. Add all of them.
(mapc (lambda (dir)
        (add-to-list 'load-path dir))
      (directory-files (concat user-emacs-directory
                               (file-name-as-directory "packages"))
                       'full))

(add-to-list 'custom-theme-load-path
             (concat user-emacs-directory
                     (file-name-as-directory "themes")))

(byte-recompile-directory user-emacs-directory 0)

;; When started as a GUI app on Mac OS X, Emacs doesn't pick up environment variables. I use ZSH. Change for your shell
(if (memq window-system '(mac ns))
  (progn
    (require 'exec-path-from-shell)
    (exec-path-from-shell-initialize))
  (progn
    (setq path (shell-command-to-string "source $HOME/.zshrc && printf $PATH"))
    (setenv "PATH" path)
    (setq exec-path (split-string path ":"))))

;; Keyboard shortcuts
(global-set-key [(control j)] 'join-line)

;; Reliable way to maximize the window on startup on Ubuntu. Sorta successful on a Mac with Emacs 24.
;(require 'maxframe)
;(add-hook 'window-setup-hook 'maximize-frame t)

;; I prefer tabs to be set at 4
(setq tab-width 4)

;; Trailing whitespace should be banned
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(trailing space-before-tab indentation space-after-tab))

;; Lines shouldn't be too long
(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Use spaces for indentation
(setq indent-tabs-mode nil)
(setq-default indent-tabs-mode nil)

;; I don't like to type yes. y should suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; Vim style
(set-default 'indicate-empty-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; Load up Tramp
(require 'tramp)

;; Startup IDo
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-max-prospects 10)
(defun ido-complete-hook ()
  (define-key ido-completion-map [tab] 'ido-complete))
(add-hook 'ido-setup-hook 'ido-complete-hook)

;; Setup Ctags
;(setq path-to-ctags (executable-find "ctags"))
;(setq tags-revert-without-query 1)

;; Setup magit
;(require 'magit)

;; Setup Projectile
(require 'projectile)
(projectile-global-mode)

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(require 'flymake-yaml)
(add-hook 'yaml-mode-hook 'flymake-yaml-load)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(require 'yaml-path)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map (kbd "C-c C-p") 'yaml-path/path)))

;; Modes in which Paredit should be active
(setf modes-for-paredit '(emacs-lisp lisp inferior-lisp slime slime-repl repl))

;; Paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode)
                                    "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      modes-for-paredit)

(defun turn-on-paredit () (paredit-mode 1))
(add-hook 'clojure-mode-hook 'turn-on-paredit)

;; Autopair mode
(require 'autopair)

;; Yasnippet mode - EVERYWHERE!
(require 'yasnippet)
(setq yas-snippet-dirs "~/.emacs.d/packages/yasnippet/snippets/")
(yas-global-mode 1)

;; Auto-complete
(require 'auto-complete-config)
(auto-complete-mode 1)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/dict")
;(ac-config-default)
(global-auto-complete-mode)

;; I don't want those temporary files
(setq make-backup-files nil)
(setq auto-save-default nil)
(global-auto-revert-mode t)

;; Don't confirm file creation
(setq confirm-nonexistent-file-or-buffer nil)

;; Fix copy-paste between emacs and other x programs
(setq x-select-enable-clipboard t)
(if (functionp 'x-cut-buffer-or-selection-value)
    (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;; Android mode
(require 'android-mode)

;; Use the silver searcher
(require 'ag)
(setq ag-highlight-search t)
;; I like to bind the *-at-point commands to F5 and F6:
(global-set-key (kbd "<f5>") 'ag-project)
(global-set-key (kbd "<f6>") 'ag-regexp-project-at-point)

;; Use cperl-mode instead of perl-mode
(mapc (lambda (pair)
        (if (eq (cdr pair) 'perl-mode)
            (setcdr pair 'cperl-mode)))
      (append auto-mode-alist interpreter-mode-alist))

;; Volatile highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Customize powerline
(require 'powerline)
(powerline-default-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-compression-mode t)
 '(column-number-mode t)
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes (quote (zenburn)))
 '(custom-safe-themes (quote ("b9df72e7a22e3fdf85eb6c5d80dfee43a28d3e643ed07a987f08105bbcdd6174" default)))
 '(debug-on-error t)
 '(display-time-mode t)
 '(font-lock-maximum-decoration t)
 '(global-font-lock-mode t)
 '(global-hl-line-mode t)
 '(global-linum-mode 1)
 '(hl-line-mode 1 t)
 '(inhibit-startup-screen t)
 '(mouse-yank-at-point t)
 '(recentf-mode 1)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(visible-bell t))

(add-hook 'emacs-startup-hook
          (lambda ()
            (delete-other-windows)
            (message "Dear %s, Emacs is ready for thy bidding... " (getenv "USER"))) t)

;; Make the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 4))

;; Uniquify buffers this way
(require 'uniquify)
(setq
    uniquify-buffer-name-style 'reverse
    uniquify-separator ":")

;; Move to the previous or next window
;; Copied from http://nex-3.com/posts/45-efficient-window-switching-in-emacs
(defun select-next-window ()
  "Switch to the next window"
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window"
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "M-<right>") 'select-next-window)
(global-set-key (kbd "M-<left>")  'select-previous-window)

;; Writegood mode
(require 'writegood-mode)
(global-set-key "\C-cg" 'writegood-mode)

;; Rainbow mode - Highlights colors within a file, such as "#FF00FF"
;; or "rgba(1,2,3,0.5)"
(require 'rainbow-mode)

;; Rainbow delimiters
(require 'rainbow-delimiters)
; In Emacs 24+
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

;; CSS mode
(autoload 'css-mode "css-mode" nil t)
(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)
                            (rainbow-mode)))

;; Sass-mode
(require 'sass-mode)
(autoload 'sass-mode "sass-mode" nil t)

;; Expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Load up Golang mode
(require 'go-mode)

;; Load up SML mode
(autoload 'sml-mode  "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
(setq auto-mode-alist
      (cons '("\.sml$" . sml-mode)
            (cons '("\.sig$" . sml-mode)
                  auto-mode-alist)))
(setq sml-program-name "sml")

;; Gist!
(require 'gist)

;; Add flymake-cursor for better errors
(eval-after-load 'flymake '(require 'flymake-cursor))

;; Smart operator prettifies operators
(require 'smart-operator)
(add-hook 'prog-mode-hook 'smart-operator-mode)

;; Multiple cursors
(require 'multiple-cursors)

;; Load up Scala, Erlang, Elixir, Ruby, JavaScript, CoffeeScript, XML, Clojure and Python  settings
(require 'scala-settings)
(require 'erlang-settings)
(require 'ruby-settings)
(require 'javascript-settings)
(require 'xml-settings)
(require 'clojure-settings)
(require 'python-settings)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal)))))
