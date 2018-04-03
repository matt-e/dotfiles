;; Configuration almost exclusively from https://github.com/cpaulik/dotfiles/blob/master/.spacemacs-config/config-layers/research-config/packages.el


(defvar research-packages
  '())
;;     parsebib
;;     helm-bibtex
;;     reftex
;;     hydra
;;     key-chord
;;     interleave
;;     org-ref
;;     ebib))

;; (defvar research-excluded-packages '())

;; (defun research/init-helm-bibtex()
;;   (use-package helm-bibtex
;;     :defer t
;;     :commands helm-bibtex
;;     :init
;;     (progn
;;       (evil-leader/set-key "or" 'helm-bibtex))
;;     :config
;;     (progn
;;       (setq helm-bibtex-bibliography "~/Dropbox/Papers/bibliography.bib")
;;       (setq helm-bibtex-library-path "~/Dropbox/Papers/pdf")
;;       (setq helm-bibtex-notes-path "~/Dropbox/Papers/notes")
;;       (setq helm-bibtex-notes-extension ".org")
;;       (setq helm-bibtex-pdf-open-function
;;             (lambda (fpath)
;;               (start-process "open" "*open*" "open" fpath)))
;;       (setq helm-bibtex-format-citation-functions
;;             (quote
;;              ((org-mode . helm-bibtex-format-citation-cite)
;;               (latex-mode . helm-bibtex-format-citation-cite)
;;               (tex-mode . helm-bibtex-format-citation-cite)
;;               (markdown-mode . helm-bibtex-format-citation-pandoc-citeproc)
;;               (default . helm-bibtex-format-citation-default))))

;;       (setq helm-bibtex-additional-search-fields '(keywords journal))

;;       (defun helm-bibtex-interleave-edit-notes (key)
;;         "Open the notes associated with the entry using `find-file'."
;;         (let ((path (f-join helm-bibtex-notes-path (s-concat key helm-bibtex-notes-extension))))
;;           (find-file path)
;;           (unless  (file-exists-p path)
;;             (insert (concat "#+INTERLEAVE_PDF: " (f-join helm-bibtex-library-path (s-concat key ".pdf")))))))

;;       (helm-delete-action-from-source "Edit notes" helm-source-bibtex)
;;       (helm-add-action-to-source "Edit notes" 'helm-bibtex-interleave-edit-notes helm-source-bibtex 7)
;;       )
;;     )
;;   )

;; (defun research/init-org-ref()
;;   "Init org ref package"
;;   (use-package org-ref
;;     :defer t
;;     :config

;;     (require 'org-ref-bibtex)
;;     (setq org-ref-bibliography-notes "~/Dropbox/Papers/notes/notes.org")
;;     (setq org-ref-default-bibliography '("~/Dropbox/Papers/bibliography.bib"))
;;     (setq org-ref-pdf-directory "~/Dropbox/Papers/pdf/")
;;     (setq reftex-default-bibliography '("~/Dropbox/Papers/bibliography.bib"))
;;     ))

;; (defun research/init-hydra ()
;;   "Init hydra package"
;;   (use-package hydra
;;     :defer t))

;; (defun research/init-key-chord ()
;;   (use-package key-chord
;;     :defer t))

;; (defun research/init-interleave ()
;;   (use-package interleave
;;     :commands interleave
;;     :defer t
;;     ))

;; (defun research/init-ebib ()
;;   (use-package ebib
;;     :defer t
;;     ))
