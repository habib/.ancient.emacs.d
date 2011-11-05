;; Haml mode
(require 'haml-mode)

;; RVM
(require 'rvm)
(rvm-use-default)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

;; (defun create-project-tags ()
;;   "Create tags file."
;;   (interactive)
;;   (shell-command
;;    (format "%s -f %s/TAGS -e -R %s" path-to-ctags (textmate-project-root) (directory-file-name (textmate-project-root)))))

;; Ruby mode settings
(add-hook 'ruby-mode-hook
	  (lambda()
	    (add-hook 'local-write-file-hooks
		      '(lambda()
			 (save-excursion
			   (untabify (point-min) (point-max))
			   (delete-trailing-whitespace))))
	    (set (make-local-variable 'indent-tabs-mode) 'nil)
	    (set (make-local-variable 'tab-width) 2)
	    (imenu-add-to-menubar "IMENU")
	    (setq ruby-deep-indent-paren nil)
	    (setq c-tab-always-indent nil)
	    (setq ruby-deep-arglist t)
	    ;(require 'ruby-style)
	    ;; Ruby block mode
;;	    (require 'ruby-block)
;;	    (ruby-block-mode t)
	    ;; display to minibuffer and do overlay
;;	    (setq ruby-block-highlight-toggle t)
;;	    (require 'inf-ruby)
;;	    (require 'ruby-compilation)
	    (define-key ruby-mode-map "\C-m" 'newline-and-indent)
;;	    (unless (file-exists-p (concat (textmate-project-root) "/TAGS"))
;;	      (create-project-tags))
))

;; ;; Install mode-compile to give friendlier compiling support!
;; (require 'mode-compile)
;; (require 'mode-compile-kill)
;; (autoload 'mode-compile "mode-compile"
;;   "Command to compile current buffer file based on the major mode" t)
;; (global-set-key "\C-cc" 'mode-compile)
;; (autoload 'mode-compile-kill "mode-compile"
;;   "Command to kill a compilation launched by `mode-compile'" t)
;; (global-set-key "\C-ck" 'mode-compile-kill)

;; ;;; FIX ME
;; ;;Rspec mode
;; ;(require 'rspec-mode)
;; ;(setq rspec-use-rvm t)

;; ;;; FIX ME
;; ;; Shoulda mode
;; ;(require 'shoulda-mode)

;; ;; rhtml mode
;; (require 'rhtml-mode)
;; ; put rhtml templates into rhtml-mode
;; (setq auto-mode-alist  (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
;; ; put any rjs scripts into ruby-mode, as they are basically ruby
;; (setq auto-mode-alist  (cons '("\\.rjs$" . ruby-mode) auto-mode-alist))

;; ;; Rinari mode
;; (require 'rinari)
;; (setq rinari-tags-file-name "TAGS")
;; (setq rinari-major-modes
;;       (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
;;             'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; ;; Cucumber feature
;; ;; language if .feature doesn't have "# language: fi"
;; (setq feature-default-language "en")
;; (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
;; (require 'feature-mode)
;; (yas/load-directory "~/.emacs.d/packages/feature-mode/snippets")

 (eval-after-load 'ruby-mode
   '(progn
      (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)))

;; ;; RSense
;; (setq rsense-home (expand-file-name "~/opt/rsense-0.3"))
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)

;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

(provide 'ruby-settings)
