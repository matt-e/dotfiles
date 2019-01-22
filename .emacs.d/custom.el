;; Auto-generated
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-adaptive-mode t nil (helm-adaptive))
 '(org-html-validation-link nil)
 '(org-todo-keywords '((sequence "TODO" "WORKING" "HOLD" "|" "DONE")))
 '(org-agenda-files '("~/Sync/org"))
 '(org-agenda-ndays 7)
 '(org-deadline-warning-days 14)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-reverse-note-order t)
 '(org-capture-templates
      '(("t" "Todo" entry (file+headline "unfiled.org" "Unfiled Tasks")
         "* TODO %?\n" :prepend t :kill-buffer t))
      '(("n" "Note" entry (file+headline "notes.org" "Notes")
         "* %u %?\n" :prepend t :kill-buffer t))))
