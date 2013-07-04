;;Basic setup
(require 'ruby-mode)
(require 'ruby-hash-syntax)
(require 'flymake)
(require 'flymake-ruby)
(require 'yari)

(setq ruby-use-encoding-map nil)

;; Haml mode
(require 'haml-mode)

;; RVM - it is still here if you need it.
;(require 'rvm)
;(rvm-use-default)

;; RBENV
(require 'rbenv)
(global-rbenv-mode)

(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

;; For electric goodness!
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda ()
                            (unless (derived-mode-p 'prog-mode)
                              (run-hooks 'prog-mode-hook))
                            (require 'ruby-electric)
                            (ruby-electric-mode t)))

(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     (setq ruby-use-encoding-map nil)
     (require 'inf-ruby)
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (inf-ruby-switch-setup)
     (add-hook 'ruby-mode-hook 'flymake-ruby-load)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")
     (require 'ruby-tools)
     (ruby-tools-mode +1)
     (require 'ruby-block)
     (ruby-block-mode t)
     (add-hook 'ruby-mode-hook 'ri-bind-key)
     (let ((m ruby-mode-map))
       (define-key m [S-f7] 'ruby-compilation-this-buffer)
       (define-key m [f7] 'ruby-compilation-this-test)
       (define-key m [f6] 'recompile))))

;(defalias 'ri 'yari)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rxml$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.irbrc$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Kirkfile$" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

;; (eval-after-load 'ruby-compilation
;;   '(progn
;;      (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
;;        (let ((comp-buffer-name (format "*%s*" name)))
;;          (when (get-buffer comp-buffer-name)
;;            (with-current-buffer comp-buffer-name
;;              (delete-region (point-min) (point-max))))))
;;      (ad-activate 'ruby-do-run-w/compilation)))

;; (eval-after-load 'ruby-mode
;;   '(progn
;;      (require 'flymake)
;;      ;; Invoke ruby with '-c' to get syntax checking
;;      (defun flymake-ruby-init ()
;;        (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                           'flymake-create-temp-inplace))
;;               (local-file (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;          (list "ruby" (list "-c" local-file))))

;;      (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
;;      (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

;;      (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
;;            flymake-err-line-patterns)

;;      (add-hook 'ruby-mode-hook
;;                (lambda ()
;;                  (when (and buffer-file-name
;;                             (file-writable-p
;;                              (file-name-directory buffer-file-name))
;;                             (file-writable-p buffer-file-name))
;;                    (local-set-key (kbd "C-c d")
;;                                   'flymake-display-err-menu-for-current-line)
;;                    (flymake-mode t))))))

;; Robe
;; (require 'robe)
;; (eval-after-load 'ruby-mode
;;   (add-hook 'ruby-mode-hook 'robe-mode))
;; (eval-after-load 'robe
;;   (add-hook 'robe-mode-hook
;;             (lambda ()
;;               (add-to-list 'ac-sources 'ac-source-robe)
;;               (set-auto-complete-as-completion-at-point-function))))

;; rhtml mode
(require 'rhtml-mode)
; put rhtml templates into rhtml-mode
(setq auto-mode-alist  (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.rhtml$" . rhtml-mode) auto-mode-alist))
; put any rjs scripts into ruby-mode, as they are basically ruby
(setq auto-mode-alist  (cons '("\\.rjs$" . ruby-mode) auto-mode-alist))

;; FIX ME
;; Rspec mode
(require 'rspec-mode)
(setq rspec-use-rvm t)

;; Shoulda mode
(require 'shoulda-mode)
(setq shoulda-use-rvm t)

;; Cucumber feature
;; Set the default language to English if .feature doesn't have "# language: en"
(setq feature-default-language "en")
(add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
(require 'feature-mode)
(yas-load-directory (concat user-emacs-directory
                            "packages/feature-mode/snippets"))

;; Rinari mode
(require 'rinari)
(setq rinari-tags-file-name "TAGS")
(setq rinari-major-modes
      '(mumamo-after-change-major-mode-hook dired-mode-hook ruby-mode-hook css-mode-hook yaml-mode-hook javascript-mode-hook))

;; ;; RSense
;; (setq rsense-home (expand-file-name "~/opt/rsense-0.3"))
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)

;; ;; (add-hook 'ruby-mode-hook
;; ;;           (lambda ()
;; ;;             (add-to-list 'ac-sources 'ac-source-rsense-method)
;; ;;             (add-to-list 'ac-sources 'ac-source-rsense-constant)))

;; Move to ack-grep mode that you'll create
;; (defun create-project-tags ()
;;   "Create tags file."
;;   (interactive)
;;   (shell-command
;;    (format "%s -f %s/TAGS -e -R %s" path-to-ctags (textmate-project-root) (directory-file-name (textmate-project-root)))))

(provide 'ruby-settings)
