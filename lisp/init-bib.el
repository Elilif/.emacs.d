(use-package calibredb
  :ensure t
  :init
  (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "~/Documents/Calibre")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/PDF")
				  ))
  )

(use-package org-ref
  :ensure t
  :config
  (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
  (setq org-ref-default-bibliography '("~/Documents/Exported-Items.bib"))
  ;; (add-to-list 'org-ref-default-bibliography calibredb-ref-default-bibliography)
  ;; (setq org-ref-get-pdf-filename-function 'org-ref-get-mendeley-filename)
  )

(provide 'init-bib)
