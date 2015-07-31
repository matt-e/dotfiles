(setq guru-warn-only t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/el-get/powerline")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; now either el-get is `require'd already, or have been `load'ed by the
;; el-get installer.

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get                        ; el-get is self-hosting
   wanderlust
   powerline))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

(scroll-bar-mode -1)

(require 'powerline)
(powerline-center-theme)

(helm-mode 1)

(when (equal system-type 'darwin)
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  ;(setq mac-command-modifier 'super) ;; Bind apple/command to  super if you want
  ;(setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
  )

(defun my/agenda-last-week ()
  (interactive)
  (let ((org-agenda-start-day "-7d"))
    (org-agenda nil "W"))
  )
