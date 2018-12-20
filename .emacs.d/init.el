
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

;; Regular init starts here

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
			 ("gnu"   . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Enable async compilation for use-package
(async-bytecomp-package-mode 1)

;; PATH
(let ((path (shell-command-to-string ". ~/.profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path (append (split-string-and-unquote path ":") exec-path)))

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  )

;; Some term enhancement
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc)
	    '(signal
	      exit))
      (let ((buffer (process-buffer proc))) ad-do-it (kill-buffer buffer)) ad-do-it))
(ad-activate 'term-sentinel)

(defadvice ansi-term (before force-bash)
  (interactive (list "/bin/zsh")))
(ad-activate 'ansi-term)


;; Other configs
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil) ;; No lockfiles because the language servers freak out

;; Splash Screen
(setq inhibit-startup-screen t)
(setq initial-scratch-message ";; Scratch")

;; Show matching parens
(setq show-paren-delay 0)
(setq show-paren-mode  1)

;; Keybinding for term mode
(add-hook 'term-mode
	  (lambda ()
	    (global-set-key (kbd "s-v") 'term-paste)))

;; OrgMode Configs
(setq org-html-validation-link nil)
(setq org-todo-keywords '((sequence "TODO" "WORKING" "HOLD" "|" "DONE")))
;; (setq org-todo-keyword-faces '(("TODO"    . "blue")
;; 			       ("WORKING" . "yellow")
;; 			       ("HOLD"    . "red")
;; 			       ("DONE"    . "green")))

;; UI configurations
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)
(global-linum-mode 1)
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono for Powerline-11"))
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(defalias 'list-buffers 'ibuffer-other-window)

;; Visual line mode with controllable column witdth
(use-package
  visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode-hook)
  :config (progn
	    ;; (add-hook 'visual-fill-column-mode-hook #'visual-line-mode)
	    (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
	    (setq visual-fill-column-center-text t)))

;; Anzu for search matching
(use-package
  anzu
  :ensure t
  :config (global-anzu-mode 1)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; ace-window for window management
(use-package
  ace-window
  :ensure t
  :config (progn (global-set-key [remap other-window] 'ace-window)
	       (custom-set-faces '(aw-leading-char-face ((t
							  (:inherit ace-jump-face-foreground
								    :height 3.0)))))))

;; move-what-i-mean - jump to beginning of line ignoring indentation
(use-package
  mwim
  :ensure t
  :bind (("C-a" . mwim-beginning)
	 ("C-e" . mwim-end)))

;; try out emacs packages without installing them
(use-package try
  :ensure t)

;; Theme
(use-package
  doom-themes
  :ensure t
  :config (load-theme 'doom-opera t))

;; Helm
(use-package
  helm
  :ensure t
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
  helm-rg
  :ensure t)


(use-package
  helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
	 ("M-I" . helm-swoop-back-to-last-point)))

(use-package
  helm-descbinds
  :ensure t
  :config (helm-descbinds-mode))

;; Projectile
(use-package
  projectile
  :ensure t
  :init (setq projectile-require-project-root nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (progn (projectile-mode 1)
		 (projectile-global-mode)))

;; Helm Projectile
(use-package
  helm-projectile
  :ensure t
  :init (setq helm-projectile-fuzzy-match t)
  :config (helm-projectile-on))

;; All The Icons
(use-package
  all-the-icons
  :ensure t)

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; Flycheck
(use-package
  flycheck
  :ensure t
  :config (global-flycheck-mode))

;; company mode
(use-package
  company
  :ensure t
  :init (progn
	  (setq company-minimum-prefix-length 3)
	  (setq company-auto-complete nil)
	  (setq company-idle-delay 0)
	  (setq company-require-match 'never)
	  (setq company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
				    company-preview-frontend company-echo-metadata-frontend))
	  (setq tab-always-indent 'complete)
	  (defvar completion-at-point-functions-saved nil))
  :config (progn (global-company-mode 1)
		 (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
		 (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
		 (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
		 (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
		 (define-key company-mode-map [remap indent-for-tab-command]
		   'company-indent-for-tab-command)
		 (defun company-indent-for-tab-command
		     (&optional
		      arg)
		   (interactive "P")
		   (let ((completion-at-point-functions-saved completion-at-point-functions)
			 (completion-at-point-functions '(company-complete-common-wrapper)))
		     (indent-for-tab-command arg)))
		 (defun company-complete-common-wrapper ()
		   (let ((completion-at-point-functions completion-at-point-functions-saved))
		     (company-complete-common)))))

;; Powerline
;; (use-package
;;   spaceline
;;   :ensure t
;;   :init (setq powerline-default-separator 'slant)
;;   :config (progn(spaceline-emacs-theme)
;; 		(spaceline-toggle-minor-modes-off)
;; 		(spaceline-toggle-buffer-size-off)
;; 		(spaceline-toggle-evil-state-on)))

(use-package
  doom-modeline
  :ensure t
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

(use-package
  magit
  :ensure t
  :config (setq vc-handled-backends (delq 'Git vc-handled-backends)))

;; Ensure we only remove trailing whitespace
(use-package
  ws-butler
  :ensure t
  :init (ws-butler-global-mode))

(use-package
  general
  :ensure t
  :config (progn (general-define-key "C-+" 'text-scale-increase)
		 (general-define-key "C--" 'text-scale-decrease)
		 (general-define-key "M-/" 'hippie-expand)
		 (general-define-key "M-f" 'forward-to-word))) ;; Replace forward-word with forward-to-work -- skips
;; whitespace at the beginning of the line

(use-package
  crux
  :ensure t
  :bind (("C-k" . crux-smart-kill-line)
	 ("C-<backspace>" . crux-kill-line-backwards)))

(use-package
  unfill
  :ensure t
  :bind ([remap fill-paragraph] . unfill-toggle))

(use-package
  avy
  :ensure t
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)))

;; (use-package mode-icons
;;   :ensure t)

;; TODO: Other packages and things
;; [Y] wrap and unwrap lines (fill-paragraph and unfill-paragraph)
;; packages:
;;   hi-lock
;;   [N] ace-jump-mode
;;   hydra?
;;   eyebrowse
;;   [Y] avy/helm-swoop/?
;;   helm-dash
;;   volatile-highlights?
;;   beacon
;;   visual-regexp
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
  :ensure t
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
	      ("C-x t M-t" . treemacs-find-tag)))

(use-package
  treemacs-projectile
  :after treemacs
  projectile
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;
;; Language Supports ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  smartparens
  :ensure t
  :hook (prog-mode . smartparens-mode))

(use-package
  rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Language servers
(use-package
  lsp-mode
  :ensure t
  :init (add-hook 'programming-mode-hook 'lsp))

(use-package
  lsp-ui
  :ensure t
  :after lsp-mode
  :init (add-hook 'lsp-mode-hook 'lsp-ui-mode))


;; Rust
(use-package
  rust-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode)))

(use-package
  lsp-rust
  :ensure t
  :after lsp-mode
  rust-mode
  :init (add-hook 'rust-mode-hook #'lsp-rust-enable))

;; Go
(use-package
  go-mode
  :ensure t
  :init (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package
  lsp-go
  :ensure t
  :after lsp-mode
  go-mode
  :init (add-hook 'go-mode-hook #'lsp-go-enable))

(use-package
  kotlin-mode
  :ensure t)

;; (use-package
;;   flycheck-kotlin
;;   :ensure t)

(use-package
  lsp-java
  :ensure t
  :after lsp-mode
  :init (add-hook 'java-mode-hook #'lsp-java-enable))


;; Elisp
(use-package
  elisp-format
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Other misc things ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Show a dashboard at startup
(use-package dashboard
  :ensure t
  :init (progn
	  (setq dashboard-startup-banner nil)
	  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        (registers . 5))))
  :config
  (dashboard-setup-startup-hook))

;; Pretty symbols for lambda etc
(global-prettify-symbols-mode +1)

;; Enable the server by default
(server-start)

;; Profile app startup
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections." (format "%.2f seconds"
									      (float-time
									       (time-subtract
										after-init-time
										before-init-time)))
		     gcs-done)))

;; Auto-generated
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "6d589ac0e52375d311afaa745205abb6ccb3b21f6ba037104d71111e7e76a3fc" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" default)))
 '(fill-column 120)
 '(org-agenda-files (quote ("~/Documents/org/")))
 '(package-selected-packages
   (quote
    (visual-fill-column dashboard doom-modeline kotlin-mode kotlin flycheck-kotlin helm-descbinds helm-deskbinds try mwim emacs-async elisp-format helm-swoop treemacs-projectile treemacs smartparens crux ws-butler magit ivy eglot go-mode go-projectile rust-mode spaceline company flycheck helm-projectile projectile helm-rg helm doom-themes anzu use-package general))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
