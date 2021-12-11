;; init-tex.el --- Initialize tex configurations.	-*- lexical-binding: t -*-

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
(use-package tex
  :ensure auctex
  :defer  t
  :hook ((LaTeX-mode . prettify-symbols-mode)
	 (LaTeX-mode . eli/TeX-mode-hook)
	 )
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  ;; (setq-default TeX-master nil)

  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-source-correlate-start-server t
        )
  
  (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex --synctex=1%(mode)%' %t" TeX-run-TeX nil t))
  (setq TeX-command-default "XeLaTeX")

  (defun eli/TeX-mode-hook ()
    (make-local-variable 'company-backends)
    (setq company-backends '((company-capf)))
    )
  )

(use-package preview
  :after latex
  :hook ((LaTeX-mode . preview-larger-previews))
  :config
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25
			(funcall (preview-scale-from-face)))))))

(use-package lsp-latex
  :ensure t
  :hook (LaTeX-mode . lsp)
  :init
  (setq lsp-latex-texlab-executable "/usr/bin/texlab")
  ;; (setq lsp-clients-digestif-executable "/usr/bin/digestif")
  (setq lsp-tex-server 'texlab)
  )

;; CDLatex settings
(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
	 (cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind((:map cdlatex-mode-map 
		("<tab>" . cdlatex-tab))
	  (:map yas-keymap
		("<tab>" . yas-next-field-or-cdlatex)
		("TAB" . yas-next-field-or-cdlatex)))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex ()
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if (bound-and-true-p cdlatex-mode)
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

(provide 'init-tex)
