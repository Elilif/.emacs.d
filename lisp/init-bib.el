;; init-bib.el --- Initialize bib configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2019-2021 by Eli

;; Author: Eli <eli.q.qian@gmail.com>
;; URL: https://github.com/Elilif/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Calendar configuration.
;;

;;; Code:

(use-package mpv
  :defer t
  :ensure t)

(use-package org-media-note
  ;; :quelpa (org-media-note :fetcher github :repo "yuchen-lea/org-media-note")
  :load-path "~/.emacs.d/private/org-media-note"
  :defer t
  :init
  (use-package org-media-note-org-ref
    :after org-media-note)
  (setq org-media-note-use-org-ref t)
  ;; :hook (org-mode .  org-media-note-mode)
  ;; :after org
  :bind (
	 ("C-c x" . org-media-note-hydra/body))
  :config
  (setq org-media-note-screenshot-image-dir "~/Documents/org-images")
  (setq org-media-note-use-refcite-first t)
  )
(setq eli/bibliography '("/home/eli/Documents/Books/catalog.bib"
			))
(use-package oc
  :after org
  :config
  (require 'oc-csl)
  (setq org-cite-csl-styles-dir "~/Documents/styles")
  (setq org-cite-export-processors '  ((beamer natbib)
				       (latex biblatex)
				       (t csl)))
  (require 'oc-natbib)
  (require 'oc-biblatex))

(use-package citar
  :ensure t
  :after all-the-icons
  :custom
  (citar-symbols
	`((file ,(all-the-icons-faicon "file-o" :face 'all-the-icons-green :v-adjust -0.1) . " ")
          (note ,(all-the-icons-material "speaker_notes" :face 'all-the-icons-blue :v-adjust -0.3) . " ")
          (link ,(all-the-icons-octicon "link" :face
					'all-the-icons-orange :v-adjust 0.01) . " ")))
  (citar-symbol-separator "  ")
  (citar-bibliography eli/bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  (defun eli-citar-org-format-note (key entry)
    "Format a note from KEY and ENTRY."
    (let* ((template (citar--get-template 'note))
           (note-meta (when template
			(citar-format--entry template entry)))
           (filepath (expand-file-name
                      (concat key ".org")
                      (car citar-notes-paths)))
           (buffer (find-file filepath)))
      (with-current-buffer buffer
	(setq eli-tes note-meta)
	;; This just overrides other template insertion.
	(erase-buffer)
	(citar-org-roam-make-preamble key)
	(insert "#+title: ")
	(when template (insert (replace-regexp-in-string ":/home.*:PDF" (car (citar-get-files key)) note-meta)))
	)))
  (setq org-cite-global-bibliography eli/bibliography)
  (setq citar-at-point-function 'citar-dwim)
  (setq citar-note-format-function #'eli-citar-org-format-note)
  (setq citar-notes-paths '("~/Dropbox/org/roam/references"))
  (setq citar-templates
	'((main . "${author:30}     ${date year issued:4}     ${title:48}")
	 (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
	 (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
	 (note . "${title}
- bibliography :: bibliography:/home/eli/Documents/Thesis/catalog.bib
- tags :: ${tags}
- keywords :: ${keywords}

* Notes
:PROPERTIES:
:Custom_ID: ${=key=}
:URL: ${url}
:AUTHOR: ${author}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:")))
  )


(use-package citar-embark
  :ensure t
  :after citar embark
  :config (citar-embark-mode))

(use-package calibredb
  :ensure t
  :defer t
  :hook
  (calibredb-search-mode . eli/calibre-refresh)
  :init
  (autoload 'calibredb "calibredb")
  :config
  (setq calibredb-root-dir "~/Documents/Books")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/Books")
				  ))
  (defun eli/update-calibre-bibtex ()
    "Export the catalog with BibTex file."
    (interactive)
    (calibredb-command :command "catalog"
                       :option (format "%s"
                                       (shell-quote-argument
					(expand-file-name
					 (or calibredb-ref-default-bibliography
                                             (concat (file-name-as-directory calibredb-root-dir) "catalog.bib")))))
                       :input (s-join " " (-remove 's-blank? (-flatten "--fields title,authors,formats,isbn,pubdate,publisher,tags,languages")))
                       :library (format "--library-path %s" (calibredb-root-dir-quote)))
    (calibredb-ref-default-bibliography)
    (message "Updated BibTex file."))

  (defun eli/calibre-refresh ()
    (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
    )
  (add-hook 'calibredb-search-mode-hook 'eli/calibre-refresh)
  )

(use-package bibtex-completion
  :ensure t
  :defer t
  :config
  (setq bibtex-completion-bibliography eli/bibliography
	bibtex-completion-library-path "/home/eli/Documents/Thesis"
	bibtex-completion-notes-path "/home/eli/Dropbox/org/roam/references"
	bibtex-completion-pdf-field "file"
	;; bibtex-completion-notes-template-multiple-files "* ${author-or-editor}, ${title}, ${journal}, (${year}) :${=type=}: \n\nSee [[cite:&${=key=}]]\n"

	bibtex-completion-additional-search-fields '(keywords)
	bibtex-completion-display-formats
	'((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
	  (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
	  (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
	  (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
	)
  )

(use-package bibtex
  :config
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5
	bibtex-dialect 'biblatex
	)
  (setq my/primary-bibtex-biblatex-entry-alist
        '(("MastersThesis" "MastersThesis"
           (("author")
	    ("title")
	    ("school")
	    ("year")
	    ("tertiaryauthor")
	    ("keywords")
	    ("abstract")
	    ("databaseprovider")
	    ("url"))
           nil
           nil)))

  (setq bibtex-biblatex-entry-alist
        (append bibtex-biblatex-entry-alist my/primary-bibtex-biblatex-entry-alist))
  )

(use-package org-ref
  :ensure t
  :defer t
  ;; :config
  ;; (require 'org-ref-helm)
  ;; (require 'org-ref-bibtex)
  ;; (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
  ;; 	org-ref-insert-cite-function 'org-ref-cite-insert-helm
  ;; 	org-ref-insert-label-function 'org-ref-insert-label-link
  ;; 	org-ref-insert-ref-function 'org-ref-insert-ref-link
  ;; 	org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
  ;; 	)
  )

(use-package org-roam-bibtex
  :ensure t
  :defer t
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-roam-ref-format 'org-cite)
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf"))
  )

(use-package ebib
  :ensure t
  :defer t
  :config
  (setq ebib-preload-bib-files '("/home/eli/Documents/Thesis/catalog.bib")))

(provide 'init-bib)
