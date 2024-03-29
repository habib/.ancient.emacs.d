;; Our packages are placed under the packages folder. Add all of them.
(let ((default-directory (concat user-emacs-directory "packages")))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'load-path "~/.emacs.d/")

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

;; Diminish keeps the modeline tidy
(require 'diminish)

;; Keyboard shortcuts
(global-set-key [(control j)] 'join-line)

;; Bookmarks
(require 'bookmark)
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1)

;; Trailing whitespace should be banned
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq whitespace-style '(trailing space-before-tab indentation space-after-tab))

;; Lines shouldn't be too long
;; (require 'whitespace)
;; (setq whitespace-line-column 80)
;; (setq whitespace-style '(face lines-tail))
;; (add-hook 'prog-mode-hook 'whitespace-mode)
(require 'fill-column-indicator)
(add-hook 'prog-mode-hook #'(lambda ()
                              (progn
                                (setq fci-rule-column 80)
                                (fci-mode 1))))

;; Use spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; I don't like to type yes. y should suffice
(fset 'yes-or-no-p 'y-or-n-p)

;; Vim style
(set-default 'indicate-empty-lines t)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; wcheck mode for spelling and other corrections
(autoload 'wcheck-mode "wcheck-mode" "Toggle wcheck-mode." t)
(autoload 'wcheck-change-language "wcheck-mode" "Switch wcheck-mode languages." t)
(autoload 'wcheck-actions "wcheck-mode" "Open actions menu." t)
(autoload 'wcheck-jump-forward "wcheck-mode" "Move point forward to next marked text area." t)
(autoload 'wcheck-jump-backward "wcheck-mode" "Move point backward to previous marked text area." t)

(setq ispell-path (executable-find "ispell"))
(setq-default
  wcheck-language "English"
  wcheck-language-data `(("English"
                          (program . ,ispell-path)
                          (args "-l" "-a" "-d" "english")
                          (action-program . ,ispell-path)
                          (action-args "-m" "-a" "-d" "english")
                          (action-parser . wcheck-parser-ispell-suggestions))))

;; Load up Tramp
(require 'tramp)

;; Dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; Dired-x
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Startup IDo
(require 'ido)
(ido-mode t)
(ido-everywhere t)
(require 'flx-ido)
(setq ido-enable-prefix nil
      ido-use-virtual-buffers t
      ido-use-filename-at-point nil
      ido-auto-merge-work-directories-length 0
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-save-directory-list-file "~/.emacs.d/ido.hist"
      ido-max-prospects 10
      ido-default-file-method 'selected-window)

(flx-ido-mode +1)
(setq ido-use-faces nil)

(defun ido-complete-hook ()
  (define-key ido-completion-map [tab] 'ido-complete))
(add-hook 'ido-setup-hook 'ido-complete-hook)

(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/.smex-items")
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; Setup Ctags
;(setq path-to-ctags (executable-find "ctags"))
;(setq tags-revert-without-query 1)

;; Setup magit
;(require 'magit)

;; Setup Projectile
(require 'projectile)
(projectile-global-mode)
(diminish 'projectile-mode "Prjl")

;; YAML mode
(autoload 'yaml-mode "yaml-mode")
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(autoload 'flymake-yaml "flymake-yaml")
(add-hook 'yaml-mode-hook 'flymake-yaml-load)
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(autoload 'yaml-path "yaml-path")
(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map (kbd "C-c C-p") 'yaml-path/path)))

;; Quack, for Scheme
(require 'quack)

;; Explore Geiser

;; Modes in which Paredit should be active
(setf modes-for-paredit '(emacs-lisp scheme lisp inferior-lisp slime slime-repl repl clojure))

;; Paredit
(autoload 'paredit-mode "paredit" "Minor mode for pseudo-structurally editing Lisp code." t)
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode)
                                    "-mode-hook"))))
          (add-hook hook (lambda () (paredit-mode +1)))))
      modes-for-paredit)

 ;; Autopair mode
(require 'autopair)
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))

;; Yasnippet mode - EVERYWHERE!
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippets")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-go")
(yas-global-mode 1)

;; Auto-complete
(require 'auto-complete-config)
(auto-complete-mode 1)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/packages/auto-complete/dict")
;(ac-config-default)
(global-auto-complete-mode)

;; Hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

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

;; Clean up obsolete buffers automatically
(require 'midnight)

;; Android mode
(autoload 'android-mode "android-mode")

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
(diminish 'volatile-highlights-mode)

;; Customize powerline
(require 'powerline)
(powerline-default-theme)

;; Save recent files
(require 'recentf)
(setq recentf-save-file "~/.emacs.d/recentf"
      recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; Savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file "~/.emacs.d/savehist")
(savehist-mode +1)

;; Saveplace
(require 'saveplace)
(setq save-place-file "~/.emacs.d/saveplace")

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
 '(hl-line-mode 1)
 '(inhibit-startup-screen t)
 '(mouse-yank-at-point t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(menu-bar-mode nil)
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
    uniquify-after-kill-buffer-p t
    uniquify-ignore-buffers-re "^\\*")

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
(autoload 'writegood-mode "writegood-mode")
(global-set-key "\C-cg" 'writegood-mode)

;; Rainbow mode - Highlights colors within a file, such as "#FF00FF"
;; or "rgba(1,2,3,0.5)"
(autoload 'rainbow-mode "rainbow-mode")

;; Rainbow delimiters
(autoload 'rainbow-delimiters-mode "rainbow-delimiters")
; In Emacs 24+
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'paredit-mode-hook 'rainbow-delimiters-mode)

;; CSS mode
(autoload 'css-mode "css-mode" nil t)
(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-level 2)
                            (setq css-indent-offset 2)
                            (rainbow-mode)))

;; Rust mode
(autoload 'rust-mode "rust-mode")

;; Sass-mode
(autoload 'sass-mode "sass-mode" nil t)

;; Expand region
(autoload 'expand-region "expand-region")
(global-set-key (kbd "C-=") 'er/expand-region)

;; Load up Golang mode
;; go get -u github.com/dougm/goflymake
(require 'go-mode)
(require 'go-flymake)
(require 'go-flycheck)

;; Load up SML mode
(autoload 'sml-mode  "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process." t)
(setq auto-mode-alist
      (cons '("\.sml$" . sml-mode)
            (cons '("\.sig$" . sml-mode)
                  auto-mode-alist)))
(setq sml-program-name "sml")

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Gist!
(autoload 'gist "gist")

;; Add flymake-cursor for better errors
(eval-after-load 'flymake '(require 'flymake-cursor))

;; Multiple cursors
(autoload 'multiple-cursors "multiple-cursors")

;; Sensible undo
(require 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;; Load up Scala, Erlang, Elixir, Ruby, JavaScript, CoffeeScript, XML, Clojure and Python settings
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
