;; init-spell.el --- Initialize spell configurations.	-*- lexical-binding: t -*-

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


(use-package flyspell
  :defer t
  :hook ((org-mode . flyspell-mode)
	 (text-mode . flyspell-mode)
	 (emacs-lisp-mode . flyspell-prog-mode))
  :config
  (setq flyspell-mark-duplications-flag nil)
  )

(use-package ispell
  :defer t
  :config
  ;; (setq ispell-personal-dictionary "~/.emacs.d/mydictionary")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq ispell-dictionary "en")
  (defun eli-switch-dictionary()
    (interactive)
    (let* ((dic ispell-current-dictionary)
    	   (change (if (string= dic "fr") "en" "fr"))
	   (personal-dict (if (string= dic "fr") "~/.emacs.d/mydictionary" "~/.emacs.d/mydictionaryfr")))
      (ispell-change-dictionary change)
      (setq ispell-personal-dictionary personal-dict)
      (message "Dictionary switched from %s to %s" dic change)
      ))

  (global-set-key (kbd "<f9>") 'eli-switch-dictionary)
  )

(use-package flyspell-correct
  :after flyspell
  :ensure t
  :bind ((:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper))
	 (:map flyspell-mode-map ("C-." . nil))))

(use-package goldendict
  :ensure t
  :defer t
  :bind ("C-c h" . goldendict-dwim))

(use-package youdao-dictionary
  :ensure t
  :after org
  :defer t
  :bind (("C-c t" . youdao-dictionary-search-at-point-posframe))
  :config
  (setq url-automatic-caching t)
  (defun youdao-dictionary-delete-newlines (&optional beg end)
    "Save the current region (or line) to the `kill-ring' after stripping extra whitespace and new lines"
    (interactive
     (if (region-active-p)
	 (list (region-beginning) (region-end))
       (list (line-beginning-position) (line-end-position))))
    (let ((my-text (buffer-substring-no-properties beg end)))
      (with-temp-buffer
	(insert my-text)
	(goto-char 1)
	(while (looking-at "[ \t\n]")
          (delete-char 1))
	(let ((fill-column 9333999))
          (fill-region (point-min) (point-max)))
	(buffer-substring-no-properties (point-min) (point-max)))))
  (defun youdao-dictionary--region-or-word ()
    "Return word in region or word at point."
    (if (derived-mode-p 'pdf-view-mode)
	(if (pdf-view-active-region-p)
            (mapconcat 'identity (pdf-view-active-region-text) "\n"))
      (if (use-region-p)
          (youdao-dictionary-delete-newlines (region-beginning)
                                          (region-end))
	(thing-at-point (if youdao-dictionary-use-chinese-word-segmentation
                            'chinese-or-other-word
                          'word)
			t))))
  )

(use-package wordnut
  :ensure t
  :defer t
  :bind (("C-c y" . wordnut-lookup-current-word)))

(defun Eli/dict-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/txtdict/")
	(INIT-INPUT))
    (consult-ripgrep BASEDIR INIT-INPUT)))

(defun Eli/te-search ()
  (interactive)
  (let ((BASEDIR "~/Documents/TEdict")
	(INIT-INPUT "\\("))
    (consult-ripgrep BASEDIR INIT-INPUT)))


(provide 'init-spell)
