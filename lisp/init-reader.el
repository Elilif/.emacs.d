;; init-reader.el --- Initialize reader configurations.	-*- lexical-binding: t -*-

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
(use-package pdf-tools
  :ensure t
  :defer 5
  :config
  (pdf-tools-install)
  (defun +pdf-keyboard-select-region (&optional all-pages-p)
    "Ref: https://github.com/dalanicolai/dala-emacs-lisp/blob/9662aa2ab993157e6e7587385d27c48ed50a1a50/pdf-keyboard-highlight.el#L79"
    (interactive "P")
    (pdf-view-deactivate-region)
    (let* ((pages (if all-pages-p nil (pdf-view-current-page)))
	   (candidates (mapcar (lambda (x)
				 (list (cdar (cdr x))
				       (cdar x)
                                       (cdar (cddr x))))
			       (pdf-info-search-regexp (read-string "Regexp: ") pages)))
           (page-edges-list (alist-get (completing-read "Select correct context: " candidates)
                                       candidates nil nil 'equal))
	   (edges-list (cadr page-edges-list))
           (edges (append (cl-subseq (car edges-list) 0 2) (cl-subseq (car (last edges-list)) 2 4)))
	   )
      (pdf-view-goto-page (car page-edges-list))
      (setq pdf-view-active-region (list edges))
      (pdf-view--push-mark)
      (pdf-view-display-region)))

  (with-eval-after-load 'pdf-view
    (define-key pdf-view-mode-map (kbd "v") #'+pdf-keyboard-select-region))
  )

(use-package org-noter
  :ensure t
  :after org
  :custom
  (org-noter-auto-save-last-location t)
  (org-noter-doc-split-fraction '(0.52 0.48))
  (org-noter-notes-search-path '("~/Dropbox/org/roam"))
  (org-noter-always-create-frame nil)
  )

(use-package org-pdftools
  :ensure t
  :after org
  :defer 5
  :hook (org-mode . org-pdftools-setup-link))

;; (use-package org-noter-pdftools
;;   :ensure t
;;   :defer 5
;;   :after org-noter
;;   :config
;;   (setq org-noter-pdftools-use-org-id nil)
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freestyle-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))

;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((inhibit-read-only t)
;;            (ast (org-noter--parse-root))
;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;        (with-current-buffer (org-noter--session-notes-buffer session)
;;          (org-with-wide-buffer
;;           (goto-char (org-element-property :begin ast))
;;           (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;             (org-entry-put nil org-noter-property-note-location
;;                            (org-noter--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; save last pdf position
;; Recover last viewed position
(use-package saveplace-pdf-view
  :ensure t
  :after pdf-tools
  :commands (saveplace-pdf-view-find-file-advice saveplace-pdf-view-to-alist-advice)
  :init
  (advice-add 'save-place-find-file-hook :around #'saveplace-pdf-view-find-file-advice)
  (advice-add 'save-place-to-alist :around #'saveplace-pdf-view-to-alist-advice))

;; epub reader

(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(use-package justify-kp
  :defer t
  :load-path "~/.emacs.d/private/justify-kp")

(use-package nov
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (add-hook 'nov-mode-hook #'shrface-mode)
  (defun my-nov-font-setup ()
    (face-remap-add-relative 'variable-pitch :family "Alegreya"
                             :height 1.5))
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
  :config
  (require 'justify-kp)
  (setq nov-text-width t)
  (defun my-nov-window-configuration-change-hook ()
    (my-nov-post-html-render-hook)
    (remove-hook 'window-configuration-change-hook
		 'my-nov-window-configuration-change-hook
		 t))

  (defun my-nov-post-html-render-hook ()
    (if (get-buffer-window)
	(let ((max-width 80)
              buffer-read-only)
          (save-excursion
            (goto-char (pj-line-width))
            (while (not (eobp))
              (when (not (looking-at "^[[:space:]]*$"))
		(goto-char (line-end-position))
		(when (> (shr-pixel-column) max-width)
                  (goto-char (line-beginning-position))
                  (pj-justify)))
              (forward-line 1))))
      (add-hook 'window-configuration-change-hook
		'my-nov-window-configuration-change-hook
		nil t)))

  (add-hook 'nov-post-html-render-hook 'my-nov-post-html-render-hook)
  ;; FIXME: errors while opening `nov' files with Unicode characters
  (with-no-warnings
    (defun my-nov-content-unique-identifier (content)
      "Return the the unique identifier for CONTENT."
      (when-let* ((name (nov-content-unique-identifier-name content))
                  (selector (format "package>metadata>identifier[id='%s']"
                                    (regexp-quote name)))
                  (id (car (esxml-node-children (esxml-query selector content)))))
        (intern id)))
    (advice-add #'nov-content-unique-identifier :override #'my-nov-content-unique-identifier))
  (require 'shrface)
  (setq nov-shr-rendering-functions '((img . nov-render-img) (title . nov-render-title)))
  (setq nov-shr-rendering-functions (append nov-shr-rendering-functions shr-external-rendering-functions))
  )

(use-package spray
  :ensure t
  :defer t
  :commands (spray-mode)
  :custom
  (spray-wpm 200))

(use-package shrface
  :ensure t
  :defer t
  :config
  (shrface-basic)
  (shrface-trial)
  (shrface-default-keybindings) ; setup default keybindings
  (setq shrface-href-versatile t))

(provide 'init-reader)
