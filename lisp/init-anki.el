;; init-anki.el --- Initialize anki configurations.	-*- lexical-binding: t -*-

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


;; (use-package anki-editor
;;   :ensure t
;;   :defer t
;;   ;; :hook (org-mode . anki-editor-mode)
;;   :bind (:map org-mode-map
;;               ("<f12>" . anki-editor-cloze-region-auto-incr)
;;               ("<f11>" . anki-editor-cloze-region-dont-incr)
;;               ("<f10>" . anki-editor-reset-cloze-number)
;;               ("<f9>"  . anki-editor-push-tree))
;;   :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
;;   :config
;;   (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
;;         anki-editor-org-tags-as-anki-tags t)

;;   (defun anki-editor-cloze-region-auto-incr (&optional arg)
;;     "Cloze region without hint and increase card number."
;;     (interactive)
;;     (anki-editor-cloze-region my-anki-editor-cloze-number "")
;;     (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
;;     (forward-sexp))
;;   (defun anki-editor-cloze-region-dont-incr (&optional arg)
;;     "Cloze region without hint using the previous card number."
;;     (interactive)
;;     (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
;;     (forward-sexp))
;;   (defun anki-editor-reset-cloze-number (&optional arg)
;;     "Reset cloze number to ARG or 1"
;;     (interactive)
;;     (setq my-anki-editor-cloze-number (or arg 1)))
;;   (defun anki-editor-push-tree ()
;;     "Push all notes under a tree."
;;     (interactive)
;;     (anki-editor-push-notes '(4))
;;     (anki-editor-reset-cloze-number))
;;   ;; Initialize
;;   (anki-editor-reset-cloze-number)
;;   )

;; Org-capture templates
;; (setq org-my-anki-file "~/Dropbox/org/anki.org")
;; (add-to-list 'org-capture-templates
;;              '("a" "Anki basic" entry (file+headline org-my-anki-file "Dispatch Shelf")
;;               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
;; (add-to-list 'org-capture-templates
;;              '("A" "Anki cloze" entry (file+headline org-my-anki-file "Dispatch Shelf")
;;               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))

;; Allow Emacs to access content from clipboard.
(setq x-select-enable-clipboard t
      x-select-enable-primary nil)

(defun make-orgcapture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "org-capture") (window-system . x)))
  (select-frame-by-name "org-capture")
  (org-capture)
  (delete-other-windows)
  )

;; org-download
(use-package org-download
  :ensure t
  :after org
  :config
  (setq-default org-download-method 'directory
		org-download-image-dir "~/Documents/org-images"
		org-download-heading-lvl nil
		org-download-delete-image-after-download t
		org-download-screenshot-method "flameshot gui --raw > %s"
		org-download-image-org-width 800
		org-download-annotate-function (lambda (link) "") ;; Don't annotate
		))
;; org-download use buffer-local variables. Set it individually in files. Otherwise, put things flatly in misc
;; folder.

(add-hook 'dired-mode-hook 'org-download-enable)
(global-set-key (kbd "C-c l") 'org-store-link) ;; crop in X11 first, and paste within here later
;; Use #+ATTR_ORG: :width 300px to customized image display width
(setq org-image-actual-width nil)
;; org-attach method
(setq-default org-attach-method 'mv
              org-attach-auto-tag "attach"
              org-attach-store-link-p 't)
(provide 'init-anki)
