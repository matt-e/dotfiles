;; Startup optimization - set GC threshold high
(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer 5 nil
		     (lambda ()
		       (setq gc-cons-threshold gc-cons-threshold-original)
		       (setq file-name-handler-alist file-name-handler-alist-original)
		       (makunbound 'gc-cons-threshold-original)
		       (makunbound 'file-name-handler-alist-original)
		       (message "gc-cons-threshold and file-name-handler-alist restored")))

;; regular init starts here

;; Fix MacOS CMD/OPTION keys
;; Binds meta to ALT
(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta))

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
					user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously
			  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el" 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(straight-use-package 'use-package)

;; prefer utf8 encoding
(prefer-coding-system 'utf-8)

;;; store all backup and autosave files in the tmp dir
;;; http://emacsredux.com/blog/2013/05/09/keep-backup-and-auto-save-files-out-of-the-way/
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; enable async compilation for use-package
(use-package
  async
  :config (async-bytecomp-package-mode 1))

;; PATH
(let ((path (shell-command-to-string ". ~/.profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path (append (split-string-and-unquote path ":") exec-path)))

;; Some term enhancement
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc)
	    '(signal
	      exit
	      ))
      (let ((buffer (process-buffer proc))) ad-do-it (kill-buffer buffer)) ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)


;; Other configs
(setq make-backup-files nil)
(setq auto-save-default nil)
 ;; No lockfiles because the language servers freak out
(setq create-lockfiles nil)

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch")

;; Show matching parens
(setq show-paren-delay 0)
(setq show-paren-mode  1)

;; Keybinding for term mode
(add-hook 'term-mode
	  (lambda ()
	    (global-set-key (kbd "s-v") 'term-paste)
	    ))

;; ;; ;; OrgMode Configs
;; ;; (setq org-html-validation-link nil)
;; ;; (setq org-todo-keywords '((sequence "TODO" "WORKING" "HOLD" "|" "DONE")))
;; ;; ;; (setq org-todo-keyword-faces '(("TODO"    . "blue")
;; ;; ;; 			       ("WORKING" . "yellow")
;; ;; ;; 			       ("HOLD"    . "red")
;; ;; ;; 			       ("DONE"    . "green")))

;; UI configurations
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode 1)
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono for Powerline-11"))
(add-to-list 'default-frame-alist '(height . 80))
(add-to-list 'default-frame-alist '(width . 120))

(defalias 'list-buffers 'ibuffer-other-window)

;; Visual line mode with controllable column width
(use-package
  visual-fill-column
  :hook (visual-line-mode . visual-fill-column-mode-hook)
  :config (progn
	    ;; (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
	    (advice-add 'text-scale-adjust
			:after #'visual-fill-column-adjust)
	    (setq visual-fill-column-center-text t)))

;; Anzu for search matching
(use-package
  anzu
  :config (global-anzu-mode 1)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; ace-window for window management
(use-package
  ace-window
  :config (progn (global-set-key [remap other-window] 'ace-window)
		 (custom-set-faces '(aw-leading-char-face ((t
							    (:inherit ace-jump-face-foreground
								      :height 3.0
								      )))))))

;; move-what-i-mean - jump to beginning of line ignoring indentation
(use-package
  mwim
  :bind (("C-a" . mwim-beginning)
	 ("C-e" . mwim-end)))

;; Theme
(use-package
  doom-themes
  :config (load-theme 'doom-opera t))

;; Helm
(use-package
  helm
  :init (progn
	  (setq helm-M-x-fuzzy-match t)
	  (setq helm-mode-fuzzy-match t)
	  (setq helm-buffers-fuzzy-matching t)
	  (setq helm-recentf-fuzzy-match t)
	  (setq helm-locate-fuzzy-match t)
	  (setq helm-semantic-fuzzy-match t)
	  (setq helm-imenu-fuzzy-match t)
	  (setq helm-completion-in-region-fuzzy-match t)
	  (setq helm-candidate-number-list 80)
	  (setq helm-split-window-in-side-p t)
	  (setq helm-move-to-line-cycle-in-source t)
	  (setq helm-echo-input-in-header-line t)
	  (setq helm-autoresize-max-height 0)
	  (setq helm-autoresize-min-height 20))
  :config (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x r b" . helm-filtered-bookmarks)
	 ("C-x C-f" . helm-find-files)))

(use-package
  helm-rg)


(use-package
  helm-swoop
  :bind (("M-i" . helm-swoop)
	 ("M-I" . helm-swoop-back-to-last-point)))

(use-package
  helm-descbinds
  :config (helm-descbinds-mode))

;; Projectile
(use-package
  projectile
  :init (setq projectile-require-project-root nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (progn (projectile-mode 1)
		 (projectile-global-mode)))

;; Helm Projectile
(use-package
  helm-projectile
  :init (setq helm-projectile-fuzzy-match t)
  :config (helm-projectile-on))

;; All The Icons
(use-package
  all-the-icons)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; flycheck
(use-package
  flycheck
  :config (global-flycheck-mode))

;; company mode
(use-package
  company
  :init (progn
	  (setq company-minimum-prefix-length 3)
	  (setq company-auto-complete nil)
	  (setq company-idle-delay 0)
	  (setq company-require-match 'never)
	  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend company-preview-frontend company-echo-metadata-frontend))
	  (setq tab-always-indent 'complete)
	  (defvar completion-at-point-functions-saved nil
	    ))
  :config (progn (global-company-mode 1)
		 (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
		 (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
		 (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
		 (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
		 (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-for-tab-command)
		 (defun company-indent-for-tab-command
		     (&optional
		      arg
		      )
		   (interactive "P")
		   (let ((completion-at-point-functions-saved completion-at-point-functions)
			 (completion-at-point-functions '(company-complete-common-wrapper)))
		     (indent-for-tab-command arg))
		   )
		 (defun company-complete-common-wrapper ()
		   (let ((completion-at-point-functions completion-at-point-functions-saved))
		     (company-complete-common))
		   )))

;; Powerline
;; (use-package
;;   spaceline
;;;;   :init (setq powerline-default-separator 'slant)
;;   :config (progn(spaceline-emacs-theme)
;; 		(spaceline-toggle-minor-modes-off)
;; 		(spaceline-toggle-buffer-size-off)
;; 		(spaceline-toggle-evil-state-on)))

(use-package
  doom-modeline
  :defer t
  :hook (after-init . doom-modeline-init)
  :config (progn
	    (setq doom-modeline-height 25)
	    (setq doom-modeline-bar-width 3)
	    (setq doom-modeline-buffer-file-name-style 'relative-to-project)
	    ;; What executable of Python will be used (if nil nothing will be showed).
	    (setq doom-modeline-python-executable "python")

	    ;; Whether show `all-the-icons' or not (if nil nothing will be showed).
	    ;; The icons may not be showed correctly on Windows. Disable to make it work.
	    (setq doom-modeline-icon t)

	    ;; Whether show the icon for major mode. It should respect `doom-modeline-icon'.
	    (setq doom-modeline-major-mode-icon t)

	    ;; Whether display minor modes or not. Non-nil to display in mode-line.
	    (setq doom-modeline-minor-modes nil)

	    ;; Whether display perspective name or not. Non-nil to display in mode-line.
	    (setq doom-modeline-persp-name t)

	    ;; Whether display `lsp' state or not. Non-nil to display in mode-line.
	    (setq doom-modeline-lsp t)

	    ;; Whether display github notifications or not.
	    (setq doom-modeline-github t)

	    ;; The interval of checking github.
	    (setq doom-modeline-github-interval (* 30 60))))

(use-package magit)
;;  :config (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Ensure we only remove trailing whitespace
(use-package
  ws-butler
  :init (ws-butler-global-mode))

(use-package
  general
  :config (progn (general-define-key "C-+" 'text-scale-increase)
		 (general-define-key "C--" 'text-scale-decrease)
		 (general-define-key "M-/" 'hippie-expand)
		 (general-define-key "M-f" 'forward-to-word))) ;; Replace forward-word with forward-to-work -- skips
;; whitespace at the beginning of the line

(use-package
  crux
  :bind (("C-k" . crux-smart-kill-line)
	 ("C-<backspace>" . crux-kill-line-backwards)))

(use-package
  unfill
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package
  avy
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)))

(use-package
  mode-icons)

(use-package
  visual-regexp)

;; Other packages and things
;; wrap and unwrap lines (fill-paragraph and unfill-paragraph)
;; packages:
;;   hi-lock
;;   [N] ace-jump-mode
;;   hydra?
;;   eyebrowse
;;   [Y] avy/helm-swoop/?
;;   helm-dash
;;   volatile-highlights?
;;   beacon
;;   [Y] visual-regexp
;;   visual-regexp-steroids
;;   highlight-symbol?
;;   [Y] avy?
;;   origami/hideshow/vimish-fold?
;;   python, ruby, java, javascript, typescript,
;;   groovy, kotlin, markdown, ansible, docker, terraform, kubernetes
;;   helm-make
;; git-gutter
;; git-messenger
;; git-undo?
;; color-rg
;; magithub
;;

(use-package
  treemacs
  :defer t
  :init (with-eval-after-load 'winum (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config (progn (treemacs-follow-mode t)
		 (add-hook 'programming-mode-hook 'treemacs-tag-follow-mode))
  :bind (:map global-map
	      ("M-0"       . treemacs-select-window)
	      ("C-x t 1"   . treemacs-delete-other-windows)
	      ("C-x t t"   . treemacs)
	      ("C-x t B"   . treemacs-bookmark)
	      ("C-x t C-t" . treemacs-find-file)
	      ("C-x t M-t" . treemacs-find-tag)
	      ))

(use-package
  treemacs-projectile
  :after treemacs
  projectile)


;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  smartparens
  :hook (prog-mode . smartparens-mode))

(use-package
  rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Language servers
(use-package
  lsp-mode
  :init (add-hook 'programming-mode-hook 'lsp))

(use-package
  lsp-ui
  :after lsp-mode
  :init (add-hook 'lsp-mode-hook 'lsp-ui-mode))


;; Rust
(use-package
  rust-mode
  :init (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

;; (use-package
;;   lsp-rust
;;   :after rust-mode
;;   :init (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; Go
(use-package
  go-mode
  :init (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; (use-package
;;   lsp-go
;;   :after lsp-mode
;;   go-mode
;;   :init (add-hook 'go-mode-hook #'lsp-go-enable))

(use-package kotlin-mode)

;; (use-package
;;   flycheck-kotlin)

(use-package
  lsp-java
  :after lsp-mode
  :init (add-hook 'java-mode-hook #'lsp-java-enable))


;; elisp
(use-package
  elisp-format
  :defer t)

;; YAML
(use-package
  yaml-mode
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Other misc things ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Show a dashboard at startup
(use-package
  dashboard
  :init (progn
	  (setq dashboard-startup-banner nil)
	  (setq dashboard-items '((recents  . 5)
				  (bookmarks . 5)
				  (projects . 5)
				  (agenda . 5)
				  (registers . 5))))
  :config (dashboard-setup-startup-hook))

;; Pretty symbols for lambda etc
(global-prettify-symbols-mode +1)

;; Enable the server by default
(server-start)

;; Profile app startup
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections." (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time))) gcs-done)
	    ))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
