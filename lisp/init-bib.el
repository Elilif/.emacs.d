(use-package helm-bibtex
  :ensure t
  :defer 5
  :config
  (setq bibtex-completion-bibliography "~/Documents/Eli'sBooks/catalog.bib")
  (setq bibtex-completion-library-path "~/Documents/Eli'sBooks")
  (setq bibtex-completion-pdf-field "file")
  )

(use-package calibredb
  :ensure t
  :defer 5
  :after org
  :init
  (autoload 'calibredb "calibredb")
  :config
  ;; (setq calibredb-root-dir "~/Documents/Eli'sPDF")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/Calibre")
				  ("~/Documents/Eli'sPDF")
				  ("~/Documents/Eli'sBooks")
				  ))
  )

(defun eli/calibre-refresh ()
  (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  )
(add-hook 'calibredb-search-mode-hook 'eli/calibre-refresh)

(use-package org-ref
  :ensure t
  :after calibredb
  :defer 8
  :bind
  (:map org-mode-map
	("C-c \]" . nil))
  :config
  (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  (setq org-ref-default-bibliography '("~/Documents/Exported-Items.bib"))
  (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
  (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)
  )
;; (use-package quelpa-use-package
;;   :ensure t)
(use-package mpv
  :defer 5
  :ensure t)

(use-package org-media-note
  ;; :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :load-path "~/.emacs.d/private/org-media-note"
  :defer 10
  :init
  (use-package org-media-note-org-ref
    :after org-media-note)
  (setq org-media-note-use-org-ref t)
  :hook (org-mode .  org-media-note-mode)
  ;; :after org
  :bind (
	 ("C-c x" . org-media-note-hydra/body))
  :config
  (setq org-media-note-screenshot-image-dir "~/Documents/org-images")
  (setq org-media-note-use-refcite-first t)
  )

(use-package org-roam-bibtex
  :ensure t
  :defer 5
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf"))
  )
(provide 'init-bib)
