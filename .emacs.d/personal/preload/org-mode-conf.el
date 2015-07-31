;; My org-mode configuration. Mostly (actually almost completely)
;; stolen from
;; http://newartisans.com/2007/08/using-org-mode-as-a-day-planner/

;; Org-mode
(require 'org-install)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(define-key mode-specific-map [?a] 'org-agenda)

(eval-after-load "org"
  '(progn
     (define-prefix-command 'org-todo-state-map)

     (define-key org-mode-map "\C-cx" 'org-todo-state-map)

     (define-key org-todo-state-map "x"
       #'(lambda nil (interactive) (org-todo "CANCELLED")))
     (define-key org-todo-state-map "d"
       #'(lambda nil (interactive) (org-todo "DONE")))
     (define-key org-todo-state-map "f"
       #'(lambda nil (interactive) (org-todo "DEFERRED")))
     (define-key org-todo-state-map "l"
       #'(lambda nil (interactive) (org-todo "DELEGATED")))
     (define-key org-todo-state-map "s"
       #'(lambda nil (interactive) (org-todo "STARTED")))
     (define-key org-todo-state-map "w"
       #'(lambda nil (interactive) (org-todo "WAITING")))))

(eval-after-load "org-agenda"
  '(progn
     (define-key org-agenda-mode-map "\C-n" 'next-line)
     (define-key org-agenda-keymap "\C-n" 'next-line)
     (define-key org-agenda-mode-map "\C-p" 'previous-line)
     (define-key org-agenda-keymap "\C-p" 'previous-line)))

(define-key global-map [(control meta ?r)] 'org-capture)

(setq org-log-done 'note)
(setq org-log-done 'time)
(setq org-todo-keywords
      '(
        (sequence
         "TODO(t)"
         "STARTED(s@/!)"
         "WAITING(w@/!)"
         "DELEGATED(e@/!)"
         "APPT(@!)"
         "|"
         "DONE(d!)"
         "DEFERRED"
         "CANCELLED(c@)")))

;; Allows selecting up to the 5th level of headers, instead of just
;; the first level

(setq org-refile-targets '((org-agenda-files . (:maxlevel . 5))) targets)
