(setq guru-warn-only t)
(scroll-bar-mode -1)
(powerline-center-theme)

(when (equal system-type 'darwin)
   (setq mac-option-modifier 'meta) ;; Bind meta to ALT
   )

(defun my/agenda-last-week ()
  (interactive)
  (let ((org-agenda-start-day "-7d"))
    (org-agenda nil "W")))


(require 'helm-gtags)
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(require 'cc-mode)
(require 'function-args)
(fa-config-default)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(set-default 'semantic-case-fold t)

(define-key c-mode-map  [(control tab)] 'moo-complete)
(define-key c++-mode-map  [(control tab)] 'moo-complete)
(define-key c-mode-map (kbd "M-o")  'fa-show)
(define-key c++-mode-map (kbd "M-o")  'fa-show)


(require 'semantic)
(require 'company)
(require 'company-c-headers)

(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)

(semantic-mode 1)

(semantic-add-system-include "/usr/local/include")
(semantic-add-system-include "/usr/local/include/boost" 'c++-mode)

(add-to-list 'company-backends 'company-c-headers)
(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.2.1/")

(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;;; conf.el ends here
