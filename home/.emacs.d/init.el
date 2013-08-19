;; Add Marmalade as a package repo
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Add dotfiles-dir to load-path
(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path "~/.emacs.d/vendor")

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(col-highlight company concurrent crontab-mode ctable deferred django-theme edbi elisp-slime-nav epc find-file-in-project flymake flymake-coffee flymake-css flymake-haml flymake-ruby flymake-shell full-ack git-commit gnuplot graphviz-dot-mode haml-mode hideshowvis highlight-80+ idle-highlight-mode ido-better-flex idomenu ido-ubiquitous ido-yes-or-no inf-ruby jira json jtags jtags-extras magit mediawiki mo-git-blame monokai-theme org paredit pastels-on-dark-theme pysmell python python-mode rainbow-mode ruby-block ruby-end ruby-test-mode ruby-tools rvm smex sml-modeline smooth-scroll solarized-theme ssh-config-mode starter-kit starter-kit-bindings starter-kit-eshell starter-kit-js starter-kit-lisp starter-kit-ruby tango-2-theme tron-theme twilight-theme vline xml-rpc yaml-mode yasnippet zenburn-theme exec-path-from-shell)

  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; (require 'exec-path-from-shell)
; (set-exec-path-from-shell-PATH)
(exec-path-from-shell-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(coffee-tab-width 2)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (pastels-on-dark)))
 '(custom-safe-themes (quote ("5e6e19e4edeaa54ebc72430fcc25f0d0cec896a952c8b89dabcd214d1970c97a" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "d99134e7de58b8035aeaf094bf72c7ddb809eaf57a7cb370d00c1026dcca07aa" "36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" default)))
 '(fci-rule-color "#eee8d5")
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org" "~/Dropbox/org/house.org" "~/Dropbox/org/personal.org" "~/Dropbox/org/finance.org" "~/work/org/projects.org" "~/work/org/oncall.org" "~/work/org/interview.org" "~/work/org/side-tasks.org" "~/work/org/goals.org" "~/work/org/meetings.org" "~/work/org/notes.org" "~/work/org/unfiled.org" "~/Documents/Career/career-development.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Dropbox/org/notes.org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-remember-store-without-prompt t)
 '(org-remember-templates (quote ((116 "* TODO %?
  %u" "~/work/org/unfiled.org" "Tasks") (110 "* %u %?" "~/work/org/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(pandoc-binary "/usr/local/bin/pandoc")
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(ruby-use-encoding-map t)
 '(sml-modeline-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#211D1D" :foreground "#DADADA" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "apple" :family "Inconsolata"))))
 '(mumamo-background-chunk-major ((t nil)))
 '(mumamo-background-chunk-submode1 ((t nil))))


;; Clean up perl formatting
(setq cperl-continued-statement-offset: 4)
(setq cperl-brace-offset 0)
(setq cperl-close-paren-offset 0)
(setq cperl-continued-brace-offset 0)
(setq cperl-continued-statement-offset 4)
(setq cperl-extra-newline-before-brace nil)
(setq cperl-indent-level 4)
(setq cperl-indent-parens-as-block t)
(setq cperl-indent-wrt-brace nil)
(setq cperl-label-offset -4)
(setq cperl-merge-trailing-else t)

(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(module\\|class\\|def\\|do\\)" "\\(end\\|end\\|end\\|end\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))


;; Python Hook
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 2))))

(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'python-mode-hook         'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'ruby-mode-hook         'hs-minor-mode)

(setq-default indent-tabs-mode nil)

(setq ruby-deep-indent-paren nil)

(setq-default show-trailing-whitespace t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq c-default-style "BSD")

(require 'ruby-block)
(add-hook 'ruby-mode-hook
          '(lambda ()
             (ruby-block-mode t)
             ))

(load "my-org-mode.el")

; (require 'xcscope)
; (require 'cmake-mode)
; (require 'log4j-mode)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'upcase-region 'disabled nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(el-get 'sync)
(require 'sr-speedbar)
(require 'smooth-scrolling)
(require 'mediawiki)

(add-hook 'java-mode-hook
          (lambda () (setq c-basic-offset 2)))

(require 'rvm)
(rvm-use-default)

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-tab-command 'indent-for-tab-command)
(setq nrepl-popup-stacktraces nil)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'markdown-mode-hook 'turn-on-pandoc)
