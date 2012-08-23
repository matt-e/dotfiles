;; Add Marmalade as a package repo
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

;; Add dotfiles-dir to load-path
(add-to-list 'load-path dotfiles-dir)

(add-to-list 'load-path "~/.emacs.d/vendor")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector [solarized-bg red green yellow blue magenta cyan solarized-fg])
 '(column-number-mode t)
 '(custom-enabled-themes (quote (pastels-on-dark)))
 '(custom-safe-themes (quote ("5e6e19e4edeaa54ebc72430fcc25f0d0cec896a952c8b89dabcd214d1970c97a" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "6cfe5b2f818c7b52723f3e121d1157cf9d95ed8923dbc1b47f392da80ef7495d" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "72cc9ae08503b8e977801c6d6ec17043b55313cda34bcf0e6921f2f04cf2da56" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "d99134e7de58b8035aeaf094bf72c7ddb809eaf57a7cb370d00c1026dcca07aa" "36afe64261e1de73fcfadedf154e4bc2c9ec1969bde0c21798d31366897bc4d2" "30fe7e72186c728bd7c3e1b8d67bc10b846119c45a0f35c972ed427c45bacc19" default)))
 '(fci-rule-color "#eee8d5")
 '(org-agenda-custom-commands (quote (("d" todo "DELEGATED" nil) ("c" todo "DONE|DEFERRED|CANCELLED" nil) ("w" todo "WAITING" nil) ("W" agenda "" ((org-agenda-ndays 21))) ("A" agenda "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]"))) (org-agenda-ndays 1) (org-agenda-overriding-header "Today's Priority #A tasks: "))) ("u" alltodo "" ((org-agenda-skip-function (lambda nil (org-agenda-skip-entry-if (quote scheduled) (quote deadline) (quote regexp) "
]+>"))) (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files (quote ("~/Dropbox/org/todo.org")))
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
  %u" "~/Dropbox/org/todo.org" "Tasks") (110 "* %u %?" "~/Dropbox/org/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(sml-modeline-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


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


(global-set-key (kbd "C-+") 'toggle-hiding)
(global-set-key (kbd "C-\\") 'toggle-selective-display)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
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

(require 'xcscope)
(require 'cmake-mode)
(require 'log4j-mode)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'upcase-region 'disabled nil)
