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


(use-package helm-bibtex
  :ensure t
  :defer t
  :config
  (setq bibtex-completion-bibliography "~/Documents/Eli'sBooks/catalog.bib")
  (setq bibtex-completion-library-path "~/Documents/Eli'sBooks")
  (setq bibtex-completion-pdf-field "file")
  )

(use-package calibredb
  :ensure t
  :defer t
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

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
	'("citekey" "title" "url" "author-or-editor" "keywords" "file")
	orb-process-file-keyword t
	orb-file-field-extensions '("pdf"))
  )
(provide 'init-bib)
