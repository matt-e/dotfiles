(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(c-default-style
   (quote
    ((c-mode . "k&r")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "k&r"))))
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("e80932ca56b0f109f8545576531d3fc79487ca35a9a9693b62bf30d6d08c9aaf" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "cd70962b469931807533f5ab78293e901253f5eeb133a46c2965359f23bfb2ea" "e24180589c0267df991cf54bf1a795c07d00b24169206106624bb844292807b9" "fc2782b33667eb932e4ffe9dac475f898bf7c656f8ba60e2276704fabb7fa63b" default)))
 '(debug-on-quit nil)
 '(erc-fill-mode t)
 '(fci-rule-color "#383838")
 '(fill-column 120)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(nxml-attribute-indent 2)
 '(nxml-child-indent 2)
 '(org-agenda-columns-add-appointments-to-effort-sum t)
 '(org-agenda-custom-commands
   (quote
    (("d" todo "DELEGATED" nil)
     ("c" todo "DONE|DEFERRED|CANCELLED" nil)
     ("w" todo "WAITING" nil)
     ("W" agenda ""
      ((org-agenda-ndays 21)))
     ("A" agenda ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote notregexp)
           "\\=.*\\[#A\\]")))
       (org-agenda-ndays 1)
       (org-agenda-overriding-header "Today's Priority #A tasks: ")))
     ("u" alltodo ""
      ((org-agenda-skip-function
        (lambda nil
          (org-agenda-skip-entry-if
           (quote scheduled)
           (quote deadline)
           (quote regexp)
           "
]+>")))
       (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
 '(org-agenda-files
   (quote
    ("~/Documents/org/" "~/Dropbox/org/" "~/Documents/org/scheduled/" "~/Documents/org/cm/")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode t)
 '(org-capture-templates
   (quote
    (("t" "Todo" entry
      (file+headline "~/Documents/org/unfiled.org" "Unfiled tasks")
      "* TODO %?
  %t
  %i")
     ("j" "Journal" entry
      (file+datetree "~/Documents/org/journal.org" "Journal")
      "* %?
  %t
  %i")
     ("r" "Review notes" entry
      (file+datetree "~/Documents/org/review-notes.org" "Review notes")
      "* %?
  %t
  %i")
     ("p" "Project ideas" entry
      (file+datetree "~/Documents/org/project-ideas.org" "Project ideas")
      "* %?
  %t
  %i"))))
 '(org-columns-default-format
   "%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %3PRIORITY %10Effort(Time){:} %6CLOCKSUM{Total}")
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "~/Documents/org/notes.org")
 '(org-directory "~/Documents/org")
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-from-is-user-regexp "\\<Eddey\\>")
 '(org-global-properties
   (quote
    (("Effort_ALL" . "0:15 0:30 1:00 2:00 3:00 4:00 8:00"))))
 '(org-journal-dir "~/Documents/org/journal")
 '(org-journal-file-format "%Y-%m-%d")
 '(org-log-done t)
 '(org-modules
   (quote
    (org-bbdb org-bibtex org-ctags org-docview org-gnus org-habit org-info org-irc org-mhe org-rmail org-w3m org-wl)))
 '(org-remember-store-without-prompt t)
 '(org-reverse-note-order t)
 '(org-src-fontify-natively t)
 '(org-tag-alist
   (quote
    ((:startgroup)
     ("pending cr" . 99)
     ("pending deployment" . 100)
     (:endgroup)
     (:startgroup)
     ("Work" . 119)
     ("Personal" . 112)
     (:endgroup))))
 '(org-todo-keywords
   (quote
    ((sequence "TODO(t)" "STARTED(s@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "APPT(@!)" "|" "DONE(d!)" "DEFERRED" "CANCELLED(c@)"))))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(safe-local-variable-values (quote ((vcl-indent-level . 2))))
 '(send-mail-function (quote mailclient-send-it))
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-color-map
   (quote
    ((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3"))))
 '(vc-annotate-very-old-color "#DC8CC3"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3F3F3F" :foreground "#DCDCCC" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "nil" :family "Menlo")))))
