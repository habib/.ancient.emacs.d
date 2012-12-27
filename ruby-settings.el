;; Haml mode
(require 'haml-mode)

;; RVM
(require 'rvm)
(rvm-use-default)

;; For electric goodness!
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda () (ruby-electric-mode t)))

(eval-after-load 'ruby-mode
  '(progn
     ;; work around possible elpa bug
     (ignore-errors (require 'ruby-compilation))
     (setq ruby-use-encoding-map nil)
     (require 'inf-ruby)
     (add-hook 'ruby-mode-hook 'inf-ruby-keys)
     (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
     (define-key ruby-mode-map (kbd "C-M-h") 'backward-kill-word)
     (define-key ruby-mode-map (kbd "C-c l") "lambda")
     (define-key ruby-mode-map (kbd "#") 'ruby-interpolate)
     (require 'ruby-block)
     (ruby-block-mode t)))

(require 'yari)
(defun ri-bind-key ()
  (local-set-key [f1] 'yari))

(add-hook 'ruby-mode-hook 'ri-bind-key)

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
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))

(add-to-list 'completion-ignored-extensions ".rbc")

(defun ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(defun ruby-insert-end ()
  "Insert \"end\" at point and reindent current line."
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(defun pcomplete/rake ()
  "Completion rules for the `ssh' command."
  (pcomplete-here (pcmpl-rake-tasks)))

(defun pcmpl-rake-tasks ()
   "Return a list of all the rake tasks defined in the current
projects.  I know this is a hack to put all the logic in the
exec-to-string command, but it works and seems fast"
   (delq nil (mapcar #'(lambda(line)
                        (if (string-match "rake \\([^ ]+\\)" line) (match-string 1 line)))
                     (split-string (shell-command-to-string "rake -T") "[\n]"))))

(defun rake (task)
  (interactive (list (completing-read "Rake (default: default): "
                                      (pcmpl-rake-tasks))))
  (shell-command-to-string (concat "rake " (if (= 0 (length task)) "default" task))))

(eval-after-load 'ruby-compilation
  '(progn
     (defadvice ruby-do-run-w/compilation (before kill-buffer (name cmdlist))
       (let ((comp-buffer-name (format "*%s*" name)))
         (when (get-buffer comp-buffer-name)
           (with-current-buffer comp-buffer-name
             (delete-region (point-min) (point-max))))))
     (ad-activate 'ruby-do-run-w/compilation)))

(eval-after-load 'ruby-mode
  '(progn
     (require 'flymake)
     ;; Invoke ruby with '-c' to get syntax checking
     (defun flymake-ruby-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "ruby" (list "-c" local-file))))

     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)

     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)
           flymake-err-line-patterns)

     (add-hook 'ruby-mode-hook
               (lambda ()
                 (when (and buffer-file-name
                            (file-writable-p
                             (file-name-directory buffer-file-name))
                            (file-writable-p buffer-file-name))
                   (local-set-key (kbd "C-c d")
                                  'flymake-display-err-menu-for-current-line)
                   (flymake-mode t))))))

;; rhtml mode
(require 'rhtml-mode)
; put rhtml templates into rhtml-mode
(setq auto-mode-alist  (cons '("\\.erb$" . rhtml-mode) auto-mode-alist))
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
(yas/load-directory (concat user-emacs-directory
                            "packages/feature-mode/snippets"))

;; ;; Rinari mode
;; (require 'rinari)
;; (setq rinari-tags-file-name "TAGS")
;; (setq rinari-major-modes
;;       (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
;;             'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

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

;;;;;;;;;;;;;;;;;;

;; ;; Ruby mode settings
;; (add-hook 'ruby-mode-hook
;; 	  (lambda()
;; 	    (add-hook 'local-write-file-hooks
;; 		      '(lambda()
;; 			 (save-excursion
;; 			   (untabify (point-min) (point-max))
;; 			   (delete-trailing-whitespace))))
;; 	    (set (make-local-variable 'indent-tabs-mode) 'nil)
;; 	    (set (make-local-variable 'tab-width) 2)
;; 	    (imenu-add-to-menubar "IMENU")
;; 	    (setq ruby-deep-indent-paren nil)
;; 	    (setq c-tab-always-indent nil)
;; 	    (setq ruby-deep-arglist t)
;; 	    (define-key ruby-mode-map "\C-m" 'newline-and-indent)
;; ;;	    (unless (file-exists-p (concat (textmate-project-root) "/TAGS"))
;; ;;	      (create-project-tags))
;; ))

;; ;; ;; Install mode-compile to give friendlier compiling support!
;; ;; (require 'mode-compile)
;; ;; (require 'mode-compile-kill)
;; ;; (autoload 'mode-compile "mode-compile"
;; ;;   "Command to compile current buffer file based on the major mode" t)
;; ;; (global-set-key "\C-cc" 'mode-compile)
;; ;; (autoload 'mode-compile-kill "mode-compile"
;; ;;   "Command to kill a compilation launched by `mode-compile'" t)
;; ;; (global-set-key "\C-ck" 'mode-compile-kill)

(provide 'ruby-settings)
