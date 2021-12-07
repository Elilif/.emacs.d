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
  :hook (org-mode .  org-media-note-mode)
  ;; :after org
  :bind (
	 ("C-c x" . org-media-note-hydra/body))
  :config
  (setq org-media-note-screenshot-image-dir "~/Documents/org-images")
  (setq org-media-note-use-refcite-first t)
  )

;; (use-package citar
;;   :ensure t
;;   ;; :bind (("C-c b" . citar-insert-citation)
;;   ;;        :map minibuffer-local-map
;;   ;;        ("M-b" . citar-insert-preset))
;;   :custom
;;   (citar-bibliography '("~/Documents/Thesis/catalog.bib"))
;;   (org-cite-insert-processor 'citar)
;;   (org-cite-follow-processor 'citar)
;;   (org-cite-activate-processor 'citar)
;;   :config
;;   (setq org-cite-global-bibliography '("~/Documents/Thesis/catalog.bib"))
;;   (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
;;   (setq citar-at-point-function 'embark-act)
;;   )

(use-package calibredb
  :ensure t
  :defer t
  :init
  (autoload 'calibredb "calibredb")
  :config
  ;; (setq calibredb-root-dir "~/Documents/Eli'sPDF")
  (setq calibredb-db-dir (expand-file-name "metadata.db" calibredb-root-dir))
  (setq calibredb-library-alist '(("~/Documents/Calibre")
				  ("~/Documents/Eli'sPDF")
				  ("~/Documents/Eli'sBooks")
				  ("~/Documents/Thesis")
				  ))
  (defun eli/calibre-refresh ()
    (setq calibredb-ref-default-bibliography (concat (file-name-as-directory calibredb-root-dir) "catalog.bib"))
    )
  (add-hook 'calibredb-search-mode-hook 'eli/calibre-refresh)
  )

(use-package helm-bibtex
  :ensure t
  :after org-ref
  :config
  (require 'bibtex-completion)
  (setq bibtex-completion-bibliography "~/Documents/Thesis/catalog.bib"
	bibtex-completion-library-path "~/Documents/Thesis"
	bibtex-completion-notes-path "~/Dropbox/org/roam/references"
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
	)
  )

(use-package org-ref
  :ensure t
  :defer t
  :config
  (require 'org-ref-helm)
  ;; (require 'org-ref-bibtex)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	org-ref-insert-cite-function 'org-ref-cite-insert-helm
	org-ref-insert-label-function 'org-ref-insert-label-link
	org-ref-insert-ref-function 'org-ref-insert-ref-link
	org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body))
	)
  )

(use-package org-roam-bibtex
  :ensure t
  :defer t
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (setq orb-roam-ref-format 'org-ref-v3)
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf"))
  )


(provide 'init-bib)
